exports.onKeyboardDown = function (eff) {
  window.onkeydown = function (e) {
    eff(e.keyCode)();
  };
};
