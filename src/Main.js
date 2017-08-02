exports.onKeyboardDown = function (eff) {
  return function () {
    window.onkeydown = function (e) {
      eff(e.keyCode)();
    };
  }
};
