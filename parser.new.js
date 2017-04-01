"use strict"
let assert = require('assert')
let colors = require('colors/safe')
let smap = require('./smap.js')
const tt = {  
  none: '(none)', 
  sp: 'sp', 
  nl: 'nl', 
  cmnt: 'cmnt', 
  num: 'num', 
  id: 'id', 
  sqstr: 'sqstr', 
  dqstr: 'dqstr', 
  re: 're', 
  sym: 'sym', 
  raw: 'raw', 
}
const is_sp = (t => t.t == tt.sp), 
is_nl = (t => t.t == tt.nl), 
is_ws = (t => t.t == tt.sp || t.t == tt.nl), 
is_cmnt = (t => t.t == tt.cmnt), 
is_num = (t => t.t == tt.num), 
is_id = (t => t.t == tt.id), 
is_str = (t => t.t == tt.sqstr || t.t == tt.dqstr), 
is_sym = (t => t.t == tt.sym), 
is_raw = (t => t.t == tt.raw), 
is_re = (t => t.t == tt.re), 
is_sol = (t => t.sol), 
is_start = (t => t.t == tt.none), 
is_end = (t => t == null), 
is_empty = (t => is_start(t) || is_end(t))
function calc_indent(s) {  
  return s.length - s.trimLeft().length;
}
function tokenize(fn, str) {  
  var src_lines = str.split('\n').concat('')
  var eof = '\0'
  var s = str + eof + eof + eof + eof + eof + eof
  var toks = [
  ]
  var sol = false
  var ln = 0
  var col = 0
  var n = str.length
  var i = 0
  var ch, ch2, ch3, ch4
  var line = {  
    prev: null, 
    fn, 
    indent: calc_indent(src_lines[0]), 
    s: src_lines[0], 
    obs: 0, 
    end_sym: '', 
  }
  var prevt = {  
    t: tt.none, 
    s: '', 
    fn, 
    ln: 0, 
    col: 0, 
    pos: 0, 
    len: 0, 
    line: line, 
    sol: false, 
    prev: null, 
    next: null
  }
  function shift() {  
    let ch0 = ch
    col++
    i++
    ch = ch2
    ch2 = ch3
    ch3 = ch4
    ch4 = s[i + 3]
    if (ch0 == '\n') {  
      ln++, col = 0
      sol = true
      let s = src_lines[ln]
      line = {  
        prev: line, 
        fn, 
        indent: calc_indent(s), 
        s, 
        obs: 0, 
        end_sym: ''
      }
    }
  }
  col = 0
  i = 0
  ch = s[0]
  ch2 = s[1]
  ch3 = s[2]
  ch4 = s[3]
  sol = true
  while (ch != eof && i < n) {  
    let t = {  
      t: tt.none, 
      s: '', 
      ss: null, 
      fn: fn, 
      ln: ln, 
      col: col, 
      pos: i, 
      len: 0, 
      line: line, 
      sol, 
      prev: prevt, 
      next: null, 
    }
    let i0 = i
    if (ch >= 'a' && ch <= 'z' || ch >= 'A' && ch <= 'Z' || ch == '$' || ch == '_') {  
      sol = false
      t.t = tt.id
      while (ch >= 'a' && ch <= 'z' || ch >= 'A' && ch <= 'Z' || 
      ch == '$' || ch == '_' || ch >= '0' && ch <= '9' || 
      ch == '-' && (ch2 != '-' || 
      ch3 >= 'a' && ch3 <= 'z' || ch3 >= 'A' && ch3 <= 'Z' || 
      ch3 == '$' || ch3 == '_' || ch3 >= '0' && ch3 <= '9')) {  
        shift()
      }
      ;
      t.s = s.slice(i0, i)
      t.line.end_sym = ''
    } else if (ch >= '0' && ch <= '9' || ch == '.' && ch2 >= '0' && ch2 <= '9') {  
      sol = false
      t.t = tt.num
      while (ch >= '0' && ch <= '9' || ch == '.') shift();
      t.s = s.slice(i0, i)
      t.line.end_sym = ''
    } else if (ch <= ' ') {  
      let wst = tt.sp
      while (ch <= ' ') {  
        if (ch == '\r' || ch == '\n') {  
          wst = tt.nl
        }
        shift()
      }
      ;
      t.s = is_nl(t) ? '\n' : ' '
      t.t = wst
    } else if (ch == "'") {  
      sol = false
      t.t = tt.sqstr
      shift()
      while (ch != eof && ch != "'") {  
        if (ch == '\\') shift()
        shift()
      }
      ;
      t.ss = s.slice(i0 + 1, i)
      shift()
      t.s = s.slice(i0, i)
      t.line.end_sym = ''
    } else if (ch == '"') {  
      sol = false
      t.t = tt.dqstr
      shift()
      while (ch != eof && ch != '"') {  
        if (ch == '\\') shift()
        shift()
      }
      ;
      t.ss = s.slice(i0 + 1, i)
      shift()
      t.s = s.slice(i0, i)
      t.line.end_sym = ''
    } else if (ch == '`') {  
      sol = false
      t.t = tt.raw
      shift()
      while (ch && ch != '`') shift();
      t.ss = s.slice(i0 + 1, i)
      shift()
      t.s = s.slice(i0, i)
      t.line.end_sym = ''
    } else if (ch == '/' && ch2 == '/') {  
      while (ch != eof && ch != '\r' && ch != '\n') shift();
      continue
    } else if (ch == '/' && ch2 == '*') {  
      let wst = tt.sp
      shift(), shift()
      while (ch != eof && (ch != '*' || ch2 != '/')) {  
        if (ch == '\n' || ch == '\r') wst = tt.nl
        shift()
      }
      ;
      shift(), shift()
      t.t = wst
    } else if (ch == '|' && ch2 == '*') {  
      let wst = tt.sp
      shift(), shift()
      while (ch != eof && (ch != '*' || ch2 != '|')) {  
        if (ch == '\n' || ch == '\r') wst = tt.nl
        shift()
      }
      ;
      shift(), shift()
      t.t = wst
    } else if (ch == '/' && ('=([:&|'.indexOf(prevt.s) >= 0 || 
    prevt.prev && is_ws(prevt) && '=([:&|'.indexOf(prevt.prev.s) >= 0)) {  
      sol = false
      t.t = tt.re
      shift()
      while (ch != eof && ch != "/") {  
        if (ch == '\\') shift()
        shift()
      }
      ;
      shift()
      while (ch != eof && (ch == 'g' || ch == 'i' || ch == 'm')) {  
        shift()
      }
      ;
      t.s = (t.ss = s.slice(i0, i))
      t.line.end_sym = ''
    } else {  
      sol = false
      t.t = tt.sym
      t.s = ch
      let symlen = 1
      if (ch4 == '=' && ch3 == '>' && ch2 == '>' && ch == '>') {  
        symlen = 4
      } else if (ch3 == '=' && (ch2 == '=' && (ch == '=' || ch == '!') || 
      ch2 == '>' && ch == '>' || ch2 == '<' && ch == '<') || 
      ch == '>' && ch2 == '>' && ch3 == '>' || 
      ch == '.' && ch2 == '.' && ch3 == '.') {  
        symlen = 3
      } else if (ch2 == '=' && (ch == '=' || ch == '<' || ch == '>' || ch == '+' || ch == '-' || ch == '*' || ch == '/' || ch == '%' || ch == '&' || ch == '^' || ch == '|' || ch == '!') || 
      ch == '&' && ch2 == '&' || ch == '|' && ch2 == '|' || 
      ch == '+' && ch2 == '+' || ch == '-' && ch2 == '-' || 
      ch == '>' && ch2 == '>' || ch == '<' && ch2 == '<' || 
      ch == '=' && ch2 == '>') {  
        symlen = 2
      }
      for (let j = 0; j < symlen-1; j++) {  
        shift()
        t.s += ch
      }
      ;
      t.line.end_sym = t.s
      shift()
    }
    t.len = i-t.pos
    prevt.next = t
    toks.push(t)
    prevt = t
  }
  ;
  return toks;
}
function dump_token(tok) {  
  log('. '.repeat(tok.line.indent) + 
  tok.t + ' ' + 
  (tok.s == '\r' ? '\\r' : (tok.s == '\n' ? '\\n' : tok.s)) + ' ' + 
  (tok.sol ? 'SOL ' : ' ') + 
  colors.gray(`line #${tok.ln} col #${tok.col} "` + tok.line.s + '"'))
}
function dump_tokens(toks) {  
  var n = toks.length
  log(n + ' tokens')
  for (var i = 0; i < n; i++) {  
    var tok = toks[i]
    dump_token(tok)
  }
  ;
}
let TokStream = function (toks) {  
  var self = this
  var n = toks.length
  var pos = 0
  var t0 = null
  this.t = null
  this.t2 = null
  this.t3 = null
  this.s = ''
  this.s2 = ''
  this.s3 = ''
  var shift = (this.shift = function () {  
    t0 = self.t
    pos++
    self.t = self.t2
    self.s = self.s2
    self.t2 = self.t3
    self.s2 = self.s3
    if (pos + 2 < n) {  
      self.t3 = toks[pos + 2]
      self.s3 = self.t3.s
    } else {  
      self.t3 = null
      self.s3 = ''
    }
    return t0;
  })
  this.skip = function (s) {  
    if (this.t) {  
      if (this.t.s == s) {  
        shift()
      } else {  
        throw error('`' + s + '` expected but `' + this.t.s + '` found')
      }
    } else {  
      throw error('`' + s + '` expected but end of stream found')
    }
  }
  this.try_skip = function (s) {  
    if (this.t && this.t.s == s) {  
      shift()
      return true;
    } else {  
      return false;
    }
  }
  this.skip_sp = function () {  
    while (self.t && is_sp(self.t)) shift();
  }
  this.skip_ws = function () {  
    while (self.t && is_ws(self.t)) shift();
  }
  this.at_id = function () {  
    return is_id(self.t);
  }
  this.id = function () {  
    if (is_id(self.t)) {  
      return shift().s;
    } else {  
      throw error('id expected but ' + ttToString(self.t.t) + ' "' + self.s + '" saw')
    }
  }
  this.at_line_start = function () {  
    return is_sol(this.t);
  }
  this.until = function (s) {  
    let str = ''
    while (self.t && self.t.s != s) str += shift().s;
    return str;
  }
  function format_message(message) {  
    var fn = self.t ? self.t.fn : (t0 ? t0.fn : 'unknown')
    var ln = self.t ? self.t.ln : (t0 ? t0.ln + '(?)' : '???')
    var line = self.t ? self.t.line.s : (t0 ? t0.line.s : 'no source available')
    var fullMessage = fn + ':' + ln + ' ' + message + '\n'
    fullMessage += line
    return fullMessage;
  }
  var warn = (this.warn = function (message) {  
    console.log('WARN ' + format_message(message))
  })
  var error = (this.error = function (message) {  
    return new Error(format_message(message));
  })
  if (n > 0) {  
    self.t = toks[0]
    self.s = self.t.s
  }
  if (n > 1) {  
    self.t2 = toks[1]
    self.s2 = self.t2.s
  }
  if (n > 2) {  
    self.t3 = toks[2]
    self.s3 = self.t3.s
  }
}
let ops = Object.create(null), 
BLOCK, 
SC, SEQ, 
SPREAD, 
FUN, ARROW, GETTER, SETTER, RET, VAR, LET, CONST, EXPORT, 
IF, ELSE, ELIF, DO, WHILE, FOR, BREAK, CONT, THROW, TRY, CATCH, FINAL, 
ASS, ADS, SUS, MUS, DIS, MOS, SLS, SRS, SAS, ANS, XOS, ORS, 
COND, ALT, 
OR, AND, 
BOR, XOR, BAND, 
EQ, IEQ, EQS, IEQS, 
LT, GT, LE, GE, IN, OF, IOF, 
SHL, SHR, SAR, 
SUM, SUB, 
MUL, DIV, MOD, 
PLS, MNS, NOT, NEG, TOF, DEL, VOID, INC, DEC, 
CALL, 
DOT, MEM, SL, SR, NEW, 
ARR, HASH, 
GRP
function reg_op(rule, pri, flags) {  
  let [  
    lsym, rsym
  ] = rule.split(' ')
  let s = lsym
  let op = {  
    s, lsym, rsym, pri
  }
  for (let key in flags) {  
    op[key] = flags[key]
  }
  ;
  if (!ops[s]) ops[s] = {
  }
  if (flags.binary) ops[s].binary = op; else if (flags.unary) {  
    ops[s].unary = op
    ops[s].unary.prefix = rsym ? false : true
  } else if (flags.ctor) ops[s].ctor = op; else if (flags.stmnt) ops[s].stmnt = op; else if (flags.group) ops[s].group = op; else throw new Error('unknown operator type')
  if (flags.alias) ops[flags.alias] = ops[s]
  return op;
}
function reg_ops() {  
  let b = {  
    binary: true
  }
  let u = {  
    unary: true
  }
  let p = {  
    postfix: true, unary: true
  }
  let s = {  
    stmnt: true
  }
  let r = {  
    binary: true, rtl: true
  }
  let c = {  
    ctor: true
  }
  let ps = {  
    postfix: true, special: true
  }
  let g = {  
    group: true
  }
  BLOCK = reg_op("{ }", -1, s)
  SC = reg_op(";", -1, b)
  SEQ = reg_op(",", 1, b)
  FUN = reg_op("function", -1, {  
    stmnt: true, alias: 'fun'
  })
  GETTER = reg_op("get", -1, {  
    stmnt: true
  })
  SETTER = reg_op("set", -1, {  
    stmnt: true
  })
  RET = reg_op("return", -1, {  
    stmnt: true, alias: 'ret'
  })
  VAR = reg_op("var", -1, s)
  LET = reg_op("let", -1, s)
  CONST = reg_op("const", -1, s)
  EXPORT = reg_op("export", -1, s)
  IF = reg_op("if", -1, s)
  ELSE = reg_op("else", -1, s)
  ELIF = reg_op("elif", -1, s)
  DO = reg_op("do", -1, s)
  WHILE = reg_op("while", -1, s)
  FOR = reg_op("for", -1, s)
  BREAK = reg_op("break", -1, s)
  CONT = reg_op("continue", -1, s)
  THROW = reg_op("throw", -1, s)
  TRY = reg_op("try", -1, s)
  CATCH = reg_op("catch", -1, s)
  FINAL = reg_op("finally", -1, s)
  SPREAD = reg_op('...', 15, u)
  ARROW = reg_op('=>', 3, b)
  ASS = reg_op("=", 3, r)
  ADS = reg_op("+=", 3, r)
  SUS = reg_op("-=", 3, r)
  MUS = reg_op("*=", 3, r)
  DIS = reg_op("/=", 3, r)
  MOS = reg_op("%=", 3, r)
  SLS = reg_op("<<=", 3, r)
  SRS = reg_op(">>=", 3, r)
  SAS = reg_op(">>>=", 3, r)
  ANS = reg_op("&=", 3, r)
  XOS = reg_op("^=", 3, r)
  ORS = reg_op("|=", 3, r)
  COND = reg_op("?", 4, r)
  ALT = reg_op(":", 2, b)
  OR = reg_op("||", 5, b)
  AND = reg_op("&&", 6, b)
  BOR = reg_op("|", 7, b)
  XOR = reg_op("^", 8, b)
  BAND = reg_op("&", 9, b)
  EQ = reg_op("==", 10, b)
  IEQ = reg_op("!=", 10, b)
  EQS = reg_op("===", 10, b)
  IEQS = reg_op("!==", 10, b)
  LT = reg_op("<", 11, b)
  GT = reg_op(">", 11, b)
  LE = reg_op("<=", 11, b)
  GE = reg_op(">=", 11, b)
  IN = reg_op("in", 11, b)
  OF = reg_op("of", 11, b)
  IOF = reg_op("instanceof", 11, b)
  SHL = reg_op("<<", 12, b)
  SHR = reg_op(">>", 12, b)
  SAR = reg_op(">>>", 12, b)
  SUM = reg_op("+", 13, b)
  SUB = reg_op("-", 13, b)
  MUL = reg_op("*", 14, b)
  DIV = reg_op("/", 14, b)
  MOD = reg_op("%", 14, b)
  PLS = reg_op('+', 15, u)
  MNS = reg_op('-', 15, u)
  NOT = reg_op("!", 15, u)
  NEG = reg_op("~", 15, u)
  TOF = reg_op("typeof", 15, u)
  DEL = reg_op("delete", 15, u)
  VOID = reg_op("void", 15, u)
  INC = reg_op("++", 15, p)
  DEC = reg_op("--", 15, p)
  CALL = reg_op("( )", 17, p)
  NEW = reg_op("new", 17, u)
  DOT = reg_op(".", 18, b)
  MEM = reg_op("[ ]", 18, p)
  ARR = reg_op('[ ]', 19, c)
  HASH = reg_op('{ }', 19, c)
  reg_op('( )', 19, g)
}
reg_ops()
function strip_ws(toks) {  
  return toks.filter(k => !is_ws(k));
}
function posttokenize(toks) {  
  let toks2 = [
  ]
  let prev_nws = null
  for (let k of toks) {  
    if (is_id(k) || is_sym(k)) {  
      if (k.s in ops) {  
        let stmnt = ops[k.s].stmnt
        if ((stmnt == GETTER || stmnt == SETTER) && 
        (k.next.s == ':' || prev_nws.s == '.' || 
        prev_nws.s == 'fun') || 
        stmnt == CATCH && k.prev.s == '.') {
        } else {  
          k.ops = ops[k.s]
        }
      }
    }
    if (!is_ws(k)) {  
      k.prev_nws = prev_nws
      toks2.push(k)
      prev_nws = k
    }
  }
  ;
  return toks2;
}
const nt = {  
  empty: '(empty)', 
  uop: 'uop', 
  pop: 'pop', 
  bop: 'bop', 
  fe: 'fe', 
  fd: 'fd', 
  getter: 'getter', 
  setter: 'setter', 
  stmnt: 'stmnt', 
  block: 'block', 
}
function copy_ast_node(a) {  
  let t = {
  }
  for (let key in a) {  
    t[key] = a[key]
  }
  ;
  return t;
}
function place_token(s, a, str) {  
  let k = s.t
  a.k = k
  a.t = k.t
  a.s = k.s
  a.pos = {  
    ln: a.k.ln, col: a.k.col
  }
  if (typeof str != 'undefined') {  
    s.skip(str)
  } else {  
    s.shift()
  }
  return a;
}
function place_op(s, a, nt, op) {  
  let str = op.lsym
  place_token(s, a, str)
  a.t = nt
  a.op = op
  return a;
}
function place_uop(s, a, op) {  
  place_op(s, a, nt.uop, op)
  a.lhs = {
  }
  return a;
}
function place_pop(s, a, op) {  
  let tmp = copy_ast_node(a)
  place_op(s, a, nt.pop, op)
  a.lhs = tmp
  a.rhs = {
  }
  return a;
}
function place_bop(s, a, op) {  
  let tmp = copy_ast_node(a)
  place_op(s, a, nt.bop, op)
  a.lhs = tmp
  a.rhs = {
  }
  return a;
}
function parse_ctor(s, a, op, i) {  
  a.t = nt.uop
  a.op = op
  a.s = op.s
  s.skip(op.lsym)
  if (s.t && s.t.s != op.rsym) {  
    a.lhs = {
    }
    parse_expr(s, a.lhs, a.lhs, 1, i)
  }
  s.skip(op.rsym)
  return a;
}
function parse_expr(s, e, a, bp, i) {  
  let prev_indent = 0
  while (s.t) {  
    let k = s.t
    if (k.line.indent < prev_indent) return ;
    prev_indent = k.line.indent
    if (a.t && !(a.t == nt.uop || 
    a.t == nt.bop) && 
    k.prev_nws && 
    k.prev_nws.next && 
    k.prev_nws.next.t == tt.nl) {  
      return ;
    }
    if (k.ops) {  
      let ops = k.ops
      if (ops.unary && (!a.t && ops.unary.prefix || 
      a.t && ops.unary.postfix)) {  
        let op = ops.unary
        let p = op.pri
        if (p < bp) return ;
        if (a.t) {  
          place_pop(s, a, op)
          if (op.rsym) {  
            parse_expr(s, a.rhs, a.rhs, 0, i)
            s.skip(op.rsym)
          }
        } else {  
          place_uop(s, a, op)
          parse_expr(s, a.lhs, a.lhs, p, i)
        }
      } else if (a.t && ops.binary) {  
        let op = ops.binary
        let p = op.pri
        if (p < bp) return ;
        if (op == ARROW) {  
          let curr_indent = s.t.line.indent
          place_bop(s, a, op)
          if (s.s == '{' || s.t.line.indent > curr_indent) {  
            parse_stmnt(s, a.rhs)
          } else {  
            parse_expr(s, a.rhs, a.rhs, p, i)
          }
        } else {  
          if (op == SEQ) {  
            if (s.t.sol && s.t.line.indent < i) {  
              return;
            }
          }
          if (op.rtl) {  
            if (p == bp) {  
              a = place_bop(s, a, op).rhs
            } else {  
              let tmp = place_bop(s, a, op).rhs
              if (op != COND) {  
                parse_expr(s, tmp, tmp, p, i)
              } else {  
                parse_expr(s, tmp, tmp, p-1, i)
                s.skip(':')
                a.cond = a.lhs
                a.lhs = a.rhs
                a.rhs = {
                }
                parse_expr(s, a.rhs, a.rhs, p-1, i)
              }
            }
          } else {  
            if (p == bp) {  
              a = place_bop(s, e, op).rhs
            } else {  
              place_bop(s, a, op)
              parse_expr(s, a, a.rhs, p, i)
            }
          }
        }
      } else if (ops.ctor && !a.t) {  
        let op = ops.ctor
        parse_ctor(s, a, op, i)
      } else if (ops.group && !a.t) {  
        s.skip('(')
        parse_expr(s, a, a, 0, i)
        s.skip(')')
        if (!a.t) a.t = nt.empty
      } else if (ops.stmnt == FUN) {  
        if (!a.t) {  
          parse_fe(s, a)
        } else {  
          return ;
        }
      } else if (ops.stmnt == GETTER || ops.stmnt == SETTER) {  
        if (!a.t) {  
          let nt_ = ops.stmnt == GETTER ? nt.getter : nt.setter
          parse_fun(s, a, ops.stmnt, nt_)
        } else {  
          return ;
        }
      } else {  
        return ;
      }
    } else if (is_num(k) || is_id(k) || is_str(k) || is_raw(k) || is_re(k)) {  
      if (a.t) {  
        return ;
      }
      place_token(s, a)
    } else {  
      return ;
    }
  }
  ;
}
function parse_fun(s, a, op, nt) {  
  place_token(s, a)
  a.t = nt
  a.op = op
  if (nt == nt.fd || is_id(s.t)) {  
    a.name = s.id()
  }
  a.cond = {
  }
  parse_expr(s, a.cond, a.cond, 0, 0)
  parse_stmnt(s, a.lhs = {
  })
}
function parse_fe(s, a) {  
  parse_fun(s, a, FUN, nt.fe)
}
function parse_fd(s, a) {  
  parse_fun(s, a, FUN, nt.fd)
}
function parse_if(s, a) {  
  let start_indent = s.t.line && s.t.line.indent
  s.shift()
  a.t = nt.stmnt
  a.op = IF
  a.cond = {
  }
  parse_expr(s, a.cond, a.cond, 0)
  parse_stmnt(s, a.lhs = {
  })
  let k = s.t
  let kk = s.t2
  if (k && k.s == ';' && kk && kk.s == ELSE.s) s.skip(';')
  if (k && k.s == ';' && kk && kk.s == ELIF.s) s.skip(';')
  let alt_indent = k && k.line && k.line.indent
  let same_indent = start_indent === alt_indent
  if (same_indent) {  
    if (s.t && s.t.s == ELSE.s) {  
      s.skip('else')
      parse_stmnt(s, a.rhs = {
      })
    } else if (s.t && s.t.s == ELIF.s) {  
      parse_if(s, a.rhs = {
      })
    }
  }
}
function parse_stmnt(s, a, indent) {  
  assert(s.t != null, 'need token for parse-stmnt')
  let k = s.t
  let ops = k.ops
  let op = ops ? ops.stmnt : null
  let line = s.t.line
  let prev_line = null
  if (s.t.prev_nws) prev_line = s.t.prev_nws.line
  if (indent >= 0 && s.t.line.indent < indent) return;
  if (prev_line && 
  line != prev_line && 
  line.indent > prev_line.indent && 
  (typeof indent == 'undefined' || 
  indent >= 0 && line.indent > indent)) {  
    a.t = nt.block
    a = (a.lhs = {
    })
    parse_indent_block(s, a, line.indent)
  } else if (op == BLOCK) {  
    s.skip('{')
    a.t = nt.block
    a = (a.lhs = {
    })
    parse_block(s, a)
    s.skip('}')
  } else if (op == FUN) {  
    parse_fd(s, a)
  } else if (op) {  
    a.t = nt.stmnt
    a.op = op
    if (op == RET) {  
      if (is_nl(s.t.next)) {  
        s.shift()
        return;
      }
      s.shift()
      a.lhs = {
      }
      parse_expr(s, a.lhs, a.lhs, 0)
    } else if (op == THROW || op == VAR || op == LET || op == CONST) {  
      s.shift()
      a.lhs = {
      }
      parse_expr(s, a.lhs, a.lhs, 0)
    } else if (op == IF) {  
      parse_if(s, a)
    } else if (op == FOR) {  
      a.cond = {
      }
      s.skip('for')
      let parens = s.try_skip('(')
      parse_stmnt(s, a.cond)
      if (!parens && a.cond.op != LET && a.cond.op != VAR) {  
        let tmp = copy_ast_node(a.cond)
        a.cond = {  
          t: nt.uop, 
          op: LET, 
          lhs: tmp
        }
      }
      if (s.try_skip(';')) {  
        a.cond.next = {
        }
        parse_stmnt(s, a.cond.next)
        s.skip(';')
        a.cond.next.next = {
        }
        parse_stmnt(s, a.cond.next.next)
      }
      if (parens) s.skip(')')
      parse_stmnt(s, a.lhs = {
      })
    } else if (op == WHILE) {  
      s.skip(op.s)
      a.cond = {
      }
      parse_expr(s, a.cond, a.cond, 0)
      parse_stmnt(s, a.lhs = {
      })
    } else if (op == BREAK) {  
      s.skip('break')
      if (is_id(s.t) && s.t2.s != ':') {  
        a.name = s.id()
      }
    } else if (op == CONT) {  
      s.skip(op.s)
    } else if (op == DO) {  
      a.cond = {
      }
      s.skip(op.s)
      parse_stmnt(s, a.lhs = {
      })
      s.skip('while')
      parse_expr(s, a.cond, a.cond, 0)
    } else if (op == TRY) {  
      s.skip(op.s)
      parse_stmnt(s, a.lhs = {
      })
      if (s.t && s.t.ops && s.t.ops.stmnt == CATCH) {  
        s.skip('catch')
        s.try_skip('(')
        a.name = s.id()
        s.try_skip(')')
        parse_stmnt(s, a.cond = {
        })
      }
      if (s.t && s.t.ops && s.t.ops.stmnt == FINAL) {  
        s.skip('finally')
        parse_stmnt(s, a.rhs = {
        })
      }
    } else if (op == EXPORT) {  
      s.shift()
      a.lhs = {
      }
      parse_stmnt(s, a.lhs)
    } else {  
      throw s.error("stmnt?")
    }
  } else {  
    parse_expr(s, a, a, 0, indent)
  }
}
function parse_block(s, a) {  
  while (s.t) {  
    let k = s.t
    if (is_id(k) && s.t2 && s.t2.op == ALT) {  
      a.label = s.shift().s
      s.skip(':')
    }
    if (s.t) {  
      let prev_tok = s.t
      parse_stmnt(s, a, -1)
      if (s.t) {  
        while (s.try_skip(';')) ;
        if (s.t == prev_tok) break
        assert(!a.next)
        if (s.t) a = (a.next = {
        })
      }
    }
  }
  ;
}
function parse_indent_block(s, a, indent) {  
  while (s.t && s.t.line.indent >= indent) {  
    let prev_tok = s.t
    parse_stmnt(s, a, indent)
    if (s.t) {  
      while (s.try_skip(';')) ;
      if (s.t == prev_tok) return ;
      if (s.t) {  
        assert(!a.next)
        a = (a.next = {
        })
      }
    }
  }
  ;
}
function set_links(pa, a, prev) {  
  a.parent = pa
  a.prev = prev
  for (let f of [  
    'cond', 'lhs', 'rhs'
  ]) {  
    let child = a[f]
    if (child) set_links(a, child, null)
  }
  ;
  if (a.next) set_links(pa, a.next, a)
}
function parse(fn, str) {  
  let toks = tokenize(fn, str)
  toks = posttokenize(toks)
  let s = new TokStream(toks)
  let ast = {
  }
  parse_block(s, ast)
  s.skip_ws()
  if (s.t) throw s.error('unparsed tokens in stream')
  set_links(null, ast)
  return ast;
}
function dump_ast(a, d) {  
  d = d || 0
  if (a) {  
    let s = '  '.repeat(d)
    if (a.label) s += a.label + ': '
    if (a.t) {  
      s += a.t
      if (a.s) {  
        s += ' ' + a.s
      } else if (a.op) {  
        s += ' ' + a.op.s
      }
      if (a.name) {  
        s += ' ' + a.name
      }
      log(s)
      if (a.cond) dump_ast(a.cond, d + 1)
      if (a.lhs) dump_ast(a.lhs, d + 1)
      if (a.rhs) dump_ast(a.rhs, d + 1)
      if (a.next && a.next.t) {  
        dump_ast(a.next, d)
      }
    } else {  
      log(s)
    }
  }
}
function OutStream() {  
  const indent_width = 2
  const one_indent_s = ' '.repeat(indent_width)
  let code = ''
  let nled = true
  let tonl = false
  let level = 0
  let aclev = 0
  let dln = 0, dcol = 0
  function do_write(s) {  
    if (aclev < level) {  
      code += one_indent_s.repeat(level - aclev)
      aclev = level
    }
    code += s
    for (let ch of s.split('')) {  
      if (ch == '\n') {  
        dln++
        dcol = 0
      } else {  
        dcol++
      }
    }
    ;
    nled = false
  }
  function do_nl() {  
    do_write('\n')
    aclev = 0
    tonl = false
    nled = true
  }
  return {  
    write: function (s) {  
      if (tonl) do_nl()
      do_write(s)
      return this;
    }, 
    hang: function (s) {  
      if (tonl) tonl = false
      do_write(s)
      return this;
    }, 
    nl: function () {  
      tonl = true
      return this;
    }, 
    indent: function () {  
      level++
      return this;
    }, 
    dedent: function () {  
      level--
      assert(level >= 0)
      return this;
    }, 
    get pos() {  
      let ln = dln, col = dcol
      let eff_aclev = aclev
      if (tonl) {  
        ln++, col = 0
        eff_aclev = 0
      }
      if (eff_aclev < level) {  
        col += indent_width * (level - eff_aclev)
      }
      return {  
        ln, col
      };
    }, 
    get code() {  
      return code.trim();
    }, 
  };
}
function op_pads(a, op) {  
  let l = ' ', r = ' '
  if (op == SEQ) l = ''
  if (op == ALT) l = ''
  if (op == DOT) l = (r = '')
  return {  
    l, r
  };
}
function op_lpad(a, op) {  
  return op_pads(a, op).l;
}
function op_rpad(a, op) {  
  return op_pads(a, op).r;
}
function unparse_fun(o, a, bp) {  
  let op = a.op
  o.write(op.s).write(' ')
  if (a.name) o.write(a.name)
  o.write('(')
  unparse_expr(o, a.cond, -1)
  o.write(') ')
  unparse_stmnt(o, a.lhs)
}
function dash_to_camel(id) {  
  let first_part = true
  return id.split('-').map(part => {  
    if (first_part) {  
      first_part = false
      return part;
    }
    if (part.length > 0) {  
      return part[0].toUpperCase() + part.substring(1);
    } else {  
      return part;
    }
  }).join('');
}
function unparse_expr(o, a, bp) {  
  let fe_parens = typeof bp == 'undefined' && 
  (a.t == nt.fe || a.t == nt.pop && a.lhs.t == nt.fe)
  if (fe_parens) o.write('(')
  let t = a.t
  let op = a.op
  if (a.k) {  
    a.dpos = o.pos
  }
  if (t in tt) {  
    o.write(a.s)
  } else if (op) {  
    let p = op.pri
    if (t == nt.uop) {  
      o.write(op.s)
      if (op.s[0] >= 'a' && op.s[0] <= 'z') {  
        o.write(' ')
      }
      if (op.ctor) {  
        p = -1
        o.nl().indent()
      }
      if (a.lhs) {  
        unparse_expr(o, a.lhs, p)
      }
      if (op.ctor) {  
        o.nl().dedent()
        o.write(op.rsym)
      } else {  
        if (op.rsym) {  
          o.hang(op.rsym)
        }
      }
    } else if (t == nt.pop) {  
      unparse_expr(o, a.lhs, p)
      o.hang(a.op.lsym)
      if (a.op.rsym) {  
        unparse_expr(o, a.rhs, -1)
        o.hang(a.op.rsym)
      }
    } else if (t == nt.bop) {  
      let op_parens = p < bp || 
      p <= bp && a.parent.op && a.parent.t == nt.bop && 
      a.parent.rhs == a
      if (op_parens) o.write('(')
      if (op == COND) {  
        unparse_expr(o, a.cond, p)
        o.write(' ? ')
        unparse_expr(o, a.lhs, p)
        o.write(' : ')
        unparse_expr(o, a.rhs, p)
      } else {  
        if (a.lhs.t != nt.empty) {  
          unparse_expr(o, a.lhs, p)
        } else {  
          o.write('()')
        }
        if (a.k && a.k.prev && a.k.prev.t == tt.nl) {  
          o.nl()
        }
        o.hang(op_lpad(a, op) + op.s + op_rpad(a, op))
        if (a.k && a.k.next && a.k.next.t == tt.nl) {  
          o.nl()
        }
        if (op == ARROW) {  
          if (a.rhs.op && a.rhs.op == HASH) {  
            o.write('(')
            unparse_expr(o, a.rhs, p)
            o.hang(')')
          } else {  
            unparse_stmnt(o, a.rhs)
          }
        } else {  
          unparse_expr(o, a.rhs, p)
        }
      }
      if (op_parens) o.hang(')')
    } else if (t == nt.stmnt) {  
      unparse_block(o, a, {  
        linear: true
      })
    } else if (t == nt.fe || t == nt.getter || t == nt.setter) {  
      unparse_fun(o, a, bp)
    } else {  
      log(a)
      o.write(' expr-op??? (' + t + ') ')
    }
  } else if (t == nt.empty) {
  } else if (!t) {
  } else {  
    log(a)
    o.write(' ??? ')
  }
  if (fe_parens) o.hang(')')
}
function unparse_stmnt(o, a) {  
  if (a.k) {  
    a.dpos = o.pos
  }
  if (a.op && a.op.stmnt && a.t != nt.fe) {  
    let op = a.op
    let p = op.pri
    if (op == VAR || op == LET || op == CONST || op == THROW) {  
      o.write(op.s)
      o.write(' ')
      unparse_expr(o, a.lhs, p)
    } else if (op == RET) {  
      o.write(op.s)
      if (a.lhs) {  
        o.write(' ')
        unparse_expr(o, a.lhs, p)
      }
    } else if (a.op == IF) {  
      o.write(op.s)
      o.write(' (')
      unparse_expr(o, a.cond, p)
      o.write(') ')
      unparse_stmnt(o, a.lhs)
      if (a.rhs) {  
        if (a.lhs.t == nt.block) {  
          o.hang(' else ')
        } else {  
          o.hang('; else ')
        }
        unparse_stmnt(o, a.rhs)
      }
    } else if (op == WHILE || op == FOR) {  
      o.write(op.s)
      o.write(' (')
      if (op == FOR) {  
        unparse_stmnt(o, a.cond)
        if (a.cond.next) {  
          o.write('; ')
          unparse_expr(o, a.cond.next, p)
          o.write('; ')
          unparse_expr(o, a.cond.next.next, p)
        }
      } else {  
        unparse_expr(o, a.cond, p)
      }
      o.write(') ')
      unparse_stmnt(o, a.lhs)
    } else if (op == DO) {  
      o.write(op.s).write(' ')
      unparse_stmnt(o, a.lhs)
      o.hang(' while (')
      unparse_expr(o, a.cond)
      o.write(')')
    } else if (op == FUN) {  
      unparse_fun(o, a, -1)
    } else if (op == BREAK || op == CONT) {  
      o.write(op.s)
    } else if (op == TRY) {  
      o.write(op.s).write(' ')
      unparse_stmnt(o, a.lhs)
      o.hang('')
      if (a.cond) {  
        if (a.name) {  
          o.hang(' catch(' + a.name + ') ')
        } else {  
          o.hang(' catch ')
        }
        unparse_stmnt(o, a.cond)
      }
      if (a.rhs) {  
        o.hang(' finally ')
        unparse_stmnt(o, a.rhs)
      }
    } else if (op == EXPORT) {  
      o.write(op.s).write(' ')
      unparse_stmnt(o, a.lhs)
    } else {  
      o.write(' ???; ')
    }
  } else if (a.t == nt.block) {  
    o.write('{').indent().nl()
    unparse_block(o, a.lhs)
    o.dedent().write('}').nl()
  } else {  
    unparse_expr(o, a)
  }
}
function unparse_block(o, a) {  
  do {  
    unparse_stmnt(o, a)
    o.nl()
  } while (a = a.next)
}
function unparse(a, opts = {
}) {  
  let res = {
  }
  let o = new OutStream()
  unparse_block(o, a)
  res.code = o.code
  if (opts.smap) {  
    res.smap = smap.build(a)
    res.code = res.smap.cmnt + '\n' + res.code
  }
  return res;
}
module.exports.tt = tt
module.exports.nt = nt
module.exports.dash_to_camel = dash_to_camel
module.exports.parse = parse
module.exports.dump_ast = dump_ast
module.exports.unparse = unparse