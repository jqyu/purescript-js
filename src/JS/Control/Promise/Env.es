exports._liftEff = eff => () => Promise.resolve(eff())

exports._catch = p => onErr => ctx => p(ctx).catch(e => onErr(e)(ctx))

exports._callCC = f => ctx => new Promise(
  (resolve, reject) => {
    f(a => ctx => resolve(a))(ctx)
      .then(resolve)
      .catch(reject)
  }
)

// slightly more efficient than the lift2 implementation
exports._par = f => a => b =>
  ctx => Promise.all([a(ctx), b(ctx)])
    .then(([a, b]) => f(a)(b))
