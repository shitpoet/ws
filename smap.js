"use strict";

let chalk = require('chalk')
let parser = require('./parser')
let {walk} = require('./walk.js')

/*function create_smgen(dfn) {
  let sourceMap = require('source-map')
  let gen = new sourceMap.SourceMapGenerator({
    file: 'm1.js',
    //sourceRoot: "http://example.com/app/js/"
  })
  return {
    map: function(sln, scol, dln, dcol) {
      gen.addMapping({
        source: "m1.ws",
        original: { line: sln, column: scol },
        generated: { line: dln, column: dcol }
      })
    },
    get code() {
      return gen.toString()
    }
  }
}

function create_inline_smgen() {
  var generator = require('inline-source-map');
  var gen = generator({ charset: 'utf-8' })
  return {
    map: function(sln, scol, dln, dcol) {
      gen.addMappings('m1.ws', [{
        original: { line: sln, column: scol } ,
        generated: { line: dln, column: dcol }
      }], { line: 1 })
    },
    get code() {
      return gen.inlineMappingUrl()
    }
  }
}*/

exports.mixed_dump = function(d, a) {
  let d_lines = d.split('\n')
  //let last_sln = -1
  let dump = []
  walk(a, function(a) {
    if (a.pos && a.dpos) {
      let sln = a.pos.ln
      //last_sln = sln
      if (!(sln in dump)) {
        dump[ sln ] = {
          sln,
          s: a.k.line.s,
          d: []
        }
      }
      let d_line = d_lines[a.dpos.ln]
      if (dump[sln].d.indexOf(d_line) < 0)
        dump[sln].d.push( d_line  )
      //a.pos.ln
      //a.dpos.ln
      //log(a.pos, a.dpos)
    }
  })
  dump.sort(function(a, b){
    if(a.sln < b.sln) return -1;
    if(a.sln > b.sln) return 1;
    return 0;
  });
  for (let line of dump) {
    if (line) {
      log(chalk.yellow( line.s  ))
      for (let d_line of line.d) {
        log(chalk.gray( d_line  ))
      }
    }
  }
}

/* not column-aware version
function build_dtos(a) {
  //let s_to_d = []
  let d_to_s = []
  walk(a, function(a) {
    if (a.pos && a.dpos) {
      let dln = a.dpos.ln
      if (dln in d_to_s) {
        let m = d_to_s[dln]
        if (a.pos.ln < m.pos.ln) {
          d_to_s[dln] = a
        }
      } else {
        d_to_s[dln] = a
      }
    }
  })
  return d_to_s
} */

function build_dtos(a) {
  let d_to_s = []
  walk(a, function(a) {
    if (a.pos && a.dpos) {

      //////////
      /*if (a.k.line.s.trim().indexOf('z') >= 0)
        log(a.pos, a.dpos)*/

      d_to_s.push( a )
    }
  })
  return d_to_s
}

function find_sfn(a) {
  let sfn
  walk(a, function(a) {
    if (a.k) {
      sfn = a.k.line.fn
      return false
    }
  })
  return sfn
}

//note: here we dont handle cases with multiple source files
/*exports.build_inline = function(a) {
  let sfn = find_sfn(a)
  if (!sfn) return {d_to_s: [], inline_url: ''}
  let d_to_s = build_dtos(a)
  var generator = require('inline-source-map');
  var gen = generator({ charset: 'utf-8' })
  for (let m of d_to_s) if (m) {
    gen.addMappings(sfn, [{
      original: { line: m.pos.ln+1, column: m.pos.col } ,
      generated: { line: m.dpos.ln+2, column: m.dpos.col }
    }])
    //, { line: 1 })
  }
  let inline_url = gen.inlineMappingUrl()
  return {
    d_to_s,
    inline_url,
  }
} */

exports.build = function(a) {
  let sfn = find_sfn(a)
  if (!sfn) {
    log('no sfn for smap generation')
    return {d_to_s: [], code: '', cmnt: ''}
  }
  let dfn = sfn.replace(/\.ws$/, '.js')
  let d_to_s = build_dtos(a)

  let sourceMap = require('source-map')
  let gen = new sourceMap.SourceMapGenerator({
    file: dfn,
    //sourceRoot: "http://example.com/app/js/"
  })

  for (let m of d_to_s) if (m) {
    gen.addMapping({
      source: sfn,
      original: { line: m.pos.ln+1, column: m.pos.col } ,
      generated: { line: m.dpos.ln+2, column: m.dpos.col }
    })
  }

  let code = gen.toString()

  var convert = require('convert-source-map');
  let cmnt = convert.fromJSON(code).toComment()

  /*log('smap cmnt for '+sfn)
  log(  convert.fromJSON(code)  )*/

  return {d_to_s, code, cmnt}
}

