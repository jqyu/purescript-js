"use strict";

var _slicedToArray = function () { function sliceIterator(arr, i) { var _arr = []; var _n = true; var _d = false; var _e = undefined; try { for (var _i = arr[Symbol.iterator](), _s; !(_n = (_s = _i.next()).done); _n = true) { _arr.push(_s.value); if (i && _arr.length === i) break; } } catch (err) { _d = true; _e = err; } finally { try { if (!_n && _i["return"]) _i["return"](); } finally { if (_d) throw _e; } } return _arr; } return function (arr, i) { if (Array.isArray(arr)) { return arr; } else if (Symbol.iterator in Object(arr)) { return sliceIterator(arr, i); } else { throw new TypeError("Invalid attempt to destructure non-iterable instance"); } }; }();

exports._liftEff = function (eff) {
  return function () {
    return Promise.resolve(eff());
  };
};

exports._catch = function (p) {
  return function (onErr) {
    return function (ctx) {
      return p(ctx).catch(function (e) {
        return onErr(e)(ctx);
      });
    };
  };
};

exports._callCC = function (f) {
  return function (ctx) {
    return new Promise(function (resolve, reject) {
      f(function (a) {
        return function (ctx) {
          return resolve(a);
        };
      })(ctx).then(resolve).catch(reject);
    });
  };
};

// slightly more efficient than the lift2 implementation
exports._par = function (f) {
  return function (a) {
    return function (b) {
      return function (ctx) {
        return Promise.all([a(ctx), b(ctx)]).then(function (_ref) {
          var _ref2 = _slicedToArray(_ref, 2);

          var a = _ref2[0];
          var b = _ref2[1];
          return f(a)(b);
        });
      };
    };
  };
};