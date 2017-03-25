function walk(a, cb) {
  if (a) {
    if (a.cond)
      if (walk(a.cond, cb)===false) return false
    if (a.lhs)
      if (walk(a.lhs, cb)===false) return false
    if (cb(a)===false) return false
    if (a.rhs)
      if (walk(a.rhs, cb)===false) return false
    if (a.next)
      if (walk(a.next, cb)===false) return false
  }
}

function walk_top(a, cb) {
  if (a) {
    if (a.cond) cb(a.cond)
    if (a.lhs) cb(a.lhs)
    if (a.rhs) cb(a.rhs)
    cb(a)
    if (a.next) walk_top(a.next, cb)
  }
}

exports.walk = walk
exports.walk_top = walk_top
