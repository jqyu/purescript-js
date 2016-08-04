exports.resolve = Promise.resolve.bind(Promise)
exports.reject = Promise.reject.bind(Promise)
exports.all = Promise.all.bind(Promise)
exports.race = Promise.race.bind(Promise)
exports.delay = n => new Promise((resolve) => setTimeout(resolve, n))

exports._runPromise = nonCanceler => p =>
  (success, error) =>
    ( p.then(success).catch(error)
    , nonCanceler
    )

exports._map = f => p => p.then(x => Promise.resolve(f(x)))

exports._apply = f_ => x_ => Promise.all([f_, x_]).then(([f,x]) => Promise.resolve(f(x)))

exports._alt = x => y => x.catch(() => y)

exports._bind = x => k => x.then(k)

// _tailRecM :: forall e a b. (Either a b -> Boolean) -> (a -> Promise e (Either a b)) -> a -> Promise e b
// hopefully this is a proper trampoline?
exports._tailRecM = isLeft => f => a => new Promise(
  (resolve, reject) => {
    let promise
    const go = acc => {
      promise = f(acc)
        .then(v => isLeft(v) ? go(v.value0) : resolve(v.value0))
        .catch(e => reject(e))
    }
    go(a)
  }
)

exports._catch = p => c => p.catch(c)

/*
exports._callCC = f => new Promise(
  (resolve, reject) => {
    f(a => new Promise).then(v => resolve(v))
  }
)
*/

exports._callCC = f => new Promise(
  (resolve, reject) => {
    f(a => resolve(a))
      .then(resolve)
      .catch(reject)
  }
)

exports._stall = f => new Promise(() => {})
