const fs = require('fs')
const vm = require('vm')
const ffi = require('ffi')

let libskim = ffi.Library('/home/ors/lab/skim/libskim.so', {
  'read_and_rewrite': [ 'string', [ 'string', 'bool' ] ]
})

function read_and_curlify(fn, expand) {
  expand = expand || false
  return libskim.read_and_rewrite(fn, expand)
}

let Throwing = new Proxy({}, {
  get: function(obj, prop) {
    throw new ReferenceError('unknown property read: '+prop);
  }/*,
  set: function(obj, prop, val) {
    throw new ReferenceError('unknown property wrote: '+prop);
  }*/
})

var tt = {
  sp: 1,
  nl: 2,
  ws: 1|2,
  cmnt: 4,
  int: 8,
  //float: 16,
  num: 8|16,
  id: 32,
  sig: 64,
  sqstr: 128,
  dqstr: 256,
  qstr: 128|256,
  sym: 512,
  raw: 1024,
  sol: 16384, // start of line
  flags: 16384
}

function ttToString(mask) {
  if (mask & tt.sol) mask -= tt.sol
  switch (mask) {
    case tt.sp: return 'sp';
    case tt.nl: return 'nl';
    case tt.ws: return 'ws';
    case tt.cmnt: return 'cmnt';
    case tt.int: return 'int';
    case tt.float: return 'float';
    case tt.num: return 'num';
    case tt.id: return 'id';
    case tt.sig: return 'sig';
    case tt.sqstr: return 'sqstr';
    case tt.dqstr: return 'dqstr';
    case tt.qstr: return 'qstr';
    case tt.sym: return 'sym';
    case tt.raw: return 'raw';
    default: throw new Error('unknown token type `'+mask+'`')
  }
}

//include('toks')

var
  is_sp = t => t.t & 1,
  is_nl = t => t.t & 2,
  is_ws = t => t.t & 3,
  is_cmnt = t => t.t & 4,
  is_int = t => t.t & 8,
  //is_float = t => t.t & 16,
  is_num = t => t.t & 24,
  is_id = t => t.t & 32,
  //is_sig = t => t.t & 64,
  //is_s_q_str = t => t.t & 128,
  //isDQStr = t => t.t & 128,
  is_qstr = t => t.t & 384,
  is_sym = t => t.t & 512,
  is_raw = t => t.t & 1024,
  is_sol = t => t.t & 16384,
  is_end = t => t==null

// these types of tokens are not recognized here:
// tt.sig, tt.scmnt
var tokenize = function(fn, str) {
  var srcLines = str.split('\n').concat('')
  var eof = '\0'
  var s = str + eof+eof+eof+eof+eof+eof
  var toks = []
  var sol = tt.sol // start of line flag
  var ln = 1
  var col = 0 // :=1 after first pre-shift
  var n = str.length
  var i = -1 // :=0 after first pre-shift
  var ch,ch2,ch3
  var line = {prev: null, fn: fn, s: srcLines[0], obs: 0, lastSym: ''}
  var prevt = {
    t: 0,
    s: '',
    fn: fn,
    ln: 1,
    col: 1,
    pos: 0,
    len: 0,
    line: line,
    prev: null,
    next: null
  }

  function shift() {
    //var ch0 = ch
    col++
    i++
    //if (i < n) {
      /*ch  = s[i]
      ch2 = s[i+1]
      ch3 = s[i+2]*/
      ch = ch2
      ch2 = ch3
      ch3 = s[i+2]
    /*} else {
      ch = eof
      ch2 = eof
      ch3 = eof
    }*/
    if (/*ch=='\r' ||*/ ch=='\n') {
      ln++, col = 1
      sol = tt.sol // start of line
      // create new line descriptor
      line = {prev: line, fn: fn, s: srcLines[ln-1], obs: 0, lastSym: ''}
    }
    //return ch0
  }

  shift() // load ch,ch2,ch3
  ch  = s[0]
  ch2 = s[1]
  ch3 = s[2]
  sol = tt.sol
  while (ch!=eof && i < n) {
    let t = {
      __proto__: Throwing,
      t: sol,
      s: '',
      ss: null,
      fn: fn,
      ln: ln,
      col: col,
      pos: i,
      len: 0,
      line: line,
      prev: prevt,
      next: null
    }
    Object.seal(t)
    //t.miss = 2

    let i0 = i

    if ( // id
      ch>='a'&&ch<='z' || ch>='A'&&ch<='Z' || ch=='$' || ch=='_'
    ) {
      sol = 0
      t.t |= tt.id
      while (
        ch>='a'&&ch<='z' || ch>='A'&&ch<='Z' ||
        ch=='$' || ch=='_' || ch>='0'&&ch<='9')
      {
        shift()
      }
      t.s = s.slice(i0,i)
      t.line.lastSym = ''
    } else if (ch>='0'&&ch<='9') {
      sol = 0
      t.t |= tt.num
      while (ch>='0'&&ch<='9' || ch=='.') shift()
      t.s = s.slice(i0,i)
      t.line.lastSym = ''
    } else if (ch<=' ') { // ws
      let wst = tt.sp
      while (ch<=' ') {
        if (ch=='\r' || ch=='\n') {
          wst = tt.nl
        }
        shift()
        //break
      }
      //t.s = is_nl(t) ? '\n' : ' '
      t.t |= wst
    } else if (ch=='`') { // template string
      sol = 0
      t.t |= tt.raw
      shift()
      while (ch && ch!='`') shift()
      t.s += s.slice(i0+1,i)
      shift()
      t.line.lastSym = ''
    } else if (ch=="'") {
      sol = 0
      t.t |= tt.sqstr
      shift()
      while (ch!=eof && ch!="'") shift()
      t.ss = s.slice(i0+1,i)
      shift()
      t.s = s.slice(i0,i)
      t.line.lastSym = ''
    } else if (ch=='"') {
      sol = 0
      t.t |= tt.dqstr
      while (ch!=eof && ch!='"') shift()
      t.ss = s.slice(i0+1,i)
      shift()
      t.s = s.slice(i0,i)
      t.line.lastSym = ''
    } else if (ch=='/' && ch2=='/') { // line cmnt
      while (ch!=eof && ch!='\r' && ch!='\n') shift()
      continue
    } else if (ch=='/' && ch2=='*') { // block cmnt
      //sol = 0
      /*if (ch3=='*') {
        t.t |= tt.cmnt
        shift() // /
        shift() // *
        shift() // *
        while (ch!=eof && (ch!='*' || ch2!='*' || ch3!='/')) {
          shift()
        }
        t.s = s.slice(i0+3,i)
        shift() // *
        shift() // *
        shift() // /
      } else {*/
        let wst = tt.sp
        shift(), shift() // / *
        while (ch!=eof && (ch!='*' || ch2!='/')) {
          if (ch=='\n' || ch=='\r') wst = tt.nl
          shift()
        }
        shift(), shift() // * /
        //while (ch<=' ') shift() // collapse ws between cmnts
        t.t |= wst
      //}
    /*} else if (ch=='<' && ch2=='!' && ch3=='-') {
      sol = 0
      t.t |= tt.cmnt
      shift(), shift(), shift(), shift() // <!--
      while (ch!=eof && (ch!='-' || ch2!='-' || ch3!='>')) {
        shift()
      }
      t.s = s.slice(i0+4,i)
      shift(), shift(), shift() // -->*/
    } else {
      sol = 0
      t.t |= tt.sym
      t.s = ch
      /*if (ch=='{') {
        var upline = t.line
        while (upline = upline.prev) {
          if (upline.lastSym==',') {
            upline.obs++
          } else {
            break
          }
        }
        t.line.obs++
      }*/
      //if (ch=='}') line.hasCB = true
      t.line.lastSym = ch
      shift()
    }

    //if (t.t & !tt.flags) {
      /*if (is_ws(t)) { // try to collapse ws
        if (is_nl(t)) {
          if (is_sp(prevt)) {
            prevt.t = tt.nl
            prevt.s = '\n'
            continue
          } else if (is_nl(prevt)) {
            continue
          }
        } else if (is_sp(t)) {
          if (is_ws(prevt)) {
            continue
          }
        }
        t.s = is_nl(t) ? '\n' : ' '
      }*/
      t.len = i-t.pos
      prevt.next = t
      toks.push(t)
      prevt = t
    //}
  }

  return toks
}

function dumpTokens(toks) {
  var n = toks.length
  log(n+' tokens')
  for (var i = 0; i < n; i++) {
    var tok = toks[i]
    log(
      tok.fn+':'+tok.ln+'.'+tok.col+' pos '+tok.pos+' len '+tok.len+' '+
      ttToString(tok.t)+' '+
      (tok.s=='\r'?'\\r':tok.s=='\n'?'\\n':tok.s)+' '+
      (tok.t&tt.sol?'SOL':'')
    )
  }
}

function add_tok_patch(patches, tok, str) { patches.push({tok, str}) }

// collect and patch imports
// only simple, non-aliasing imports are supported:
//   - import {name} from './filename.js'
//   - import {name1, name2} from 'dir/other-filename.js'
// these imports are converted to refs to globals:
//   - const   {name}  =    global
//   - const   {name1, name2}  =    global
function patch_imports_v0(toks, patches) {
  for (let tok of toks) {
    if (is_id(tok) && tok.s=='import' && is_sp(tok.next)) {
      if (tok.next.next.s == '{') {
        let nameTok = tok.next.next.next
        patch_tok(tok,'const')
        while (nameTok.next.s==',') {
          nameTok = nameTok.next.next
        }
        add_tok_patch(patches, nameTok.next.next.next, '=') // `from` -> `=`
        add_tok_patch(patches, nameTok.next.next.next.next.next, 'global') // fn -> global
      }
    }
  }
}

/*function patch_imports(toks, patches) {
  let names = []
  for (let tok of toks) {
    if (is_id(tok) && tok.s=='import' && is_sp(tok.next)) {
      let name_tok = tok.next.next
      names.push( name_tok.s )
      //log('imported mod', name_tok.s)
      //patch_tok(tok,'//import')
      add_tok_patch(patches, tok, '//import')
    }
  }
  ret names
}*/

function patch_exports(toks, patches) {
  let exports = []
  for (let tok of toks) {
    if (is_id(tok) && tok.s=='export' && is_sp(tok.next)) {
      let kw = tok.next.next.s
      if (kw=='var' || kw=='let' || kw=='function' || kw=='fun') {
        let name = tok.next.next.next.next.s
        exports.push( name )
        if (kw=='var' || kw=='let') {
          add_tok_patch(patches, tok, '__ctx.'+name) // replace `export `
          add_tok_patch(patches, tok.next, '') // replace sp
          add_tok_patch(patches, tok.next.next, '') // replace kw
          add_tok_patch(patches, tok.next.next.next, '') // replace sp
          add_tok_patch(patches, tok.next.next.next.next, '') // replace name
        } else { // fun
          add_tok_patch(patches, tok, '__ctx.'+name+'=function') // replace `export `
          add_tok_patch(patches, tok.next, '') // replace sp
          add_tok_patch(patches, tok.next.next, '') // replace kw
          add_tok_patch(patches, tok.next.next.next, '') // replace sp
          add_tok_patch(patches, tok.next.next.next.next, '') // replace name
        }
      }
    }
  }
  return exports
}

// replace `fun` with `function` %]
function patch_fun(toks, patches) {
  for (let tok of toks) {
    if ( // check if it looks like a keyword....
      is_id(tok) && tok.s=='fun' &&
      (is_ws(tok.prev) || tok.prev.s=='(' || tok.prev.s=='!' ||
      tok.prev.s == ';') &&
      tok.prev.s != '.' && tok.prev.s != '[' &&
      (is_ws(tok.next) || tok.next.s=='(')
    ) {
      add_tok_patch(patches, tok, 'function')
    }
  }
}

// add brackets around conditions (if,while,for)
// also add `let` keyword after for if none is present
function patch_ifs(toks, patches) {
  for (let tok of toks) {
    if (
      (tok.s=='if' || tok.s=='while' || tok.s=='for') &&
      (tok.next.next.s!='(')
    ) {
      let kw = tok.s
      let tok2 = tok.next
      let bs = 0, cbs = 0
      while (tok2 && tok2.s!='{' && tok.ln==tok2.ln) {
        let ch = tok2.s
        if (ch=='(') bs++
        if (ch==')') bs--
        if (ch=='{') cbs++
        if (ch=='}') cbs--
        //log('tok2?',tok2.s)
        tok2 = tok2.next
      }
      if (tok2 && bs==0 && cbs==0 && tok2.s=='{') {
        //log('patch tok2',tok2.s)
        let tok3 = tok2.next
        let patch = true
        while (tok3 && !is_nl(tok3) && tok3.s!=';') {
          if (!is_ws(tok3)) {
            patch = false
            break
          }
          tok3 = tok3.next
        }
        if (patch) {
          if (kw=='for' && tok.next.next.s!='let') {
            add_tok_patch(patches, tok, 'for (let ')
          } else {
            add_tok_patch(patches, tok, kw+'(')
          }
          add_tok_patch(patches, tok2, '){')
        }
      }
    } else if (tok.s=='elif' && tok.prev.s!='.' ) {
      add_tok_patch(patches, tok, 'else if')
    }
  }
}

// make sealed object literals throwing on missing property
function patch_seals(toks, patches) {
  for (let tok of toks) {
    if ( // `seal({`
      is_id(tok) && tok.s=='seal' &&
      tok.next.s=='(' &&
      tok.next.next.s=='{'
    ) {
      add_tok_patch(patches, tok.next.next, '{__proto__:Throwing,')
    }
  }
}

global.total_patches = 0

function apply_tok_patches(code, patches) {
  patches.sort( (a,b)=>(a.tok.pos<b.tok.pos?-1:(a.tok.pos>b.tok.pos?1:0)) )

  let n = patches.length
  global.total_patches += n
  let patched = ''
  let prev = 0
  for (let i = 0; i < n; i++) {
    let patch = patches[i]
    let pos = patch.tok.pos
    let len = patch.tok.len
    let str = patch.str
    patched += code.substr(prev, pos-prev) + str
    prev = pos+len
  }
  patched += code.substr(prev)
  /*for (let i = n-1; i >= 0; i--) {
    let patch = patches[i]
    let pos = patch.tok.pos
    let len = patch.tok.len
    let str = patch.str
    code =
      code.substr(0,pos) +
      str +
      code.substr(pos+len)
  }*/
  return patched
}

//global.read_and_curlify = read_and_curlify

function wo_ext(fn) { return fn.split('/').pop() }

function dirname(fn) {
  let parts = fn.split('/')
  parts.pop()
  return parts.join('/')
}

let read = exports.read = function(fn) {
  fn = require.resolve(fn)
  if (!fs.existsSync(fn)) {
    console.log(fn+' not found')
    process.exit()
  }
  let mod_name = wo_ext(fn)
  let code = read_and_curlify(fn)
  let toks = tokenize(mod_name, code)
  //log(code)
  //dumpTokens(toks)
  let patches = []
  //patch_fun(toks, patches)
  //patch_ifs(toks, patches)
  //patch_imports(toks, patches)
  //let imports = patch_imports(toks, patches)
  let export_names = patch_exports(toks, patches)
  patch_seals(toks, patches)
  code = apply_tok_patches(code, patches)
  return {code, export_names}
}

let run = exports.run = function(fn) {
  let {code} = read(fn)
  code = '(function(require,__dirname){"use strict";'+code+'})';
  let script = new vm.Script(code, {
    filename: fn, displayErrors: true
  })
  return (script.runInThisContext()(require,dirname(fn)))
}

let load = exports.load = function(fn) {
  let {code, export_names} = read(fn)
  /*code += ';let exports = {};' + export_names.map(function(name) {
    return 'exports.'+name+'='+name
  }).join(';') + ';return exports'//*/
  code = '(function(require,__dirname,__ctx){"use strict";'+code+'})';
  let script = new vm.Script(code, {
    filename: fn, displayErrors: true
  })
  let exports = {}
  !script.runInThisContext()(require,dirname(fn),exports);
  return exports
}
