const isNull = x => x === null || x === undefined

exports['null'] = null

exports.isNull = isNull
exports.isNotNull = x => x !== null && x !== undefined

exports.nullable = d => f => x => isNull(x) ? d : f(x)
exports.nullable_ = d => f => x => isNull(x) ? d() : f(x)

exports._map = f => x => isNull(x) ? null : f(x)

exports._apply = f => x => isNull(f) || isNull(x) ? null : f(x)

exports._pure = x => x

exports._alt = x => y => isNull(x) ? y : x

exports._bind = x => k => isNull(x) ? null : k(x)

exports._extend = f => x => isNull(x) ? null : f(x)

exports._eq = eq => x => y =>
  isNull(x) ? isNull(y) // (isNull(y) ? true : false)
            : isNull(y) ? false
                        : eq(x)(y)

exports._compare = cmp => eq => lt => gt => x => y =>
  isNull(x) ? ( isNull(y) ? eq : lt )
            : ( isNull(y) ? gt : cmp(x)(y) )
