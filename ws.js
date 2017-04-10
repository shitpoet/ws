"use strict";

const fs = require('fs')
const vm = require('vm')
const path = require('path')
const toker = require('../istok/toker')
let parser = require('./parser')
let {walk, walk_top} = require('./walk')

/*function branch_of_child(pa, a) {
  for (let f of ['lhs','rhs','cond']) {
    let child = pa[f]
    if (a == child) {
      return f
    }
  }
  return false
}*/

/*function unwrap_node(a) {
  //log('unwrap node', a.t, 'parent', a.parent.t, 'parent2', a.parent.parent.t)
  let pa = a.parent
  if (pa) {
    let ppa = a.parent.parent
    if (ppa) {
      let f = branch_of_child(ppa, pa)
      ppa[f] = a
    } else {
      throw new Error('can not unwrap - no parent parent')
    }
  } else {
    throw new Error('can not unwrap - no parent')
  }
}*/

function remove_node(a) {
  if (a.prev) {
    a.prev.next = a.next
  } else {
    /*let pa = a.parent
    let f = branch_of_child(pa, a)
    pa[f] */
  }
}

function unwrap_node(a) {
  let pa = a.parent
  for (let f of Object.keys(a)) {
    pa[f] = a[f]
  }
}

function walk_seq(a, cb) {
  if (a.op && a.op.s==',') {
    walk_seq(a.lhs, cb)
    walk_seq(a.rhs, cb)
  } else {
    cb(a)
  }
}

function patch_exports(a) {
  let export_names = []

  function cb(a) {
    if (a.op && a.op.s == 'export') {
      let lhs = a.lhs
      //log(lhs.op)
      let s = lhs.op.s
      if (s=='let' || s=='var' || s=='const') {
        walk_seq(lhs.lhs, function(a){
          export_names.push(a.lhs.s)
        })
        unwrap_node(lhs)
      } else if (s=='function') {
        export_names.push(lhs.name)
        unwrap_node(lhs)
      } else {
        throw new Error('bad export')
      }
    }
  }

  walk_top(a, cb)
  return export_names
}

function camelize(a) {
  //log('camelize')

  function camelize_str(s0) {
    let s = parser.dash_to_camel(s0)
    //if (s != s0) log('camlize '+s0+' -> '+s)
    return s
  }

  function cb(a) {
    if (a.name) {
      a.name = camelize_str(a.name)
    }
    if (a.t == parser.tt.id) {

      //a.s = parser.dash_to_camel(a.s)
      a.s = camelize_str(a.s)

    }
  }

  walk(a, cb)
}

function wo_ext(fn) { return fn.split('/').pop() }

/*function dirname(fn) {
  let parts = fn.split('/')
  parts.pop()
  return parts.join('/')
}*/

function preserve_first_comment(code, cb) {
  if (code.startsWith('//#') || code.startsWith('//!')) {
    let lines = code.split('\n')
    let cmnt = lines.shift()
    return cmnt + '\n' + cb(lines.join('\n'))
  } else {
    return code
  }
}

function wrap_code(code, before, after) {
  //log('wrap_code', code)
  return preserve_first_comment(code, function(code) {
    return before + code + after
  })
}

function read(fn, opts) {
  opts = Object.assign({
    camelize: true,
    patch_exports: true,
    smap: true
  }, opts)
  //fn = require.resolve(fn)
  if (!fs.existsSync(fn)) {
    console.log(fn+' not found')
    process.exit()
  }
  let mod_name = wo_ext(fn)
  let ast = parser.parse(fn, fs.readFileSync(fn, 'utf8'))
  if (opts.camelize)
    camelize(ast)
  let export_names = []
  if (opts.patch_exports)
    export_names = patch_exports(ast)
  let {code, smap} =
    parser.unparse(ast, {smap: opts.smap})
  for (let exp_name of export_names) {
    code += `;__ctx.${exp_name} = ${exp_name};`
  }
  return {code, smap, export_names, ast}
}

exports.read = read

function create_script(fn, code, smap) {
  let dfn = fn.replace(/\.ws$/,'.js')
  let script = new vm.Script(code, {
    filename: dfn,
    displayErrors: true,
    $__smap: smap
  })
  return script
}

let run = exports.run = function (fn) {
  let {code, smap, export_names} = read(fn)
  //code = '(function(require,__dirname){"use strict";'+code+'})';
  code = wrap_code(code,
    '(function(require,__dirname){"use strict";',
    '})')
  let script = create_script(fn, code, smap)
  return (script.runInThisContext()(require, path.dirname(fn)))
}

function load(fn) {
  let {code, smap, export_names} = read(fn)
  /*code += ';let exports = {};' + export_names.map(function(name) {
    return 'exports.'+name+'='+name
  }).join(';') + ';return exports'//*/
  code = wrap_code(code,
    '(function(require,__dirname,__ctx){with(__ctx){"use strict";',
    '}})')
  let script = create_script(fn, code, smap)
  let exports = {}
  let dirname = path.dirname(fn)
  !script.runInThisContext()(require,dirname,exports);
  return exports
}

exports.load = load

// require is not good name cuz require of node.js has cache
// and require of ws - does not
exports.require = function(fn) {
  log('ws.require is deprecated')
  return load(fn)
}

exports.replaceParser = function(new_parser) {
  parser = new_parser
}
