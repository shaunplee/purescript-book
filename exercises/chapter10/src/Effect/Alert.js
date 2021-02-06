"use strict";

exports.alert = msg => () =>
  window.alert(msg);

exports.confirm = msg => () =>
  window.confirm(msg);
