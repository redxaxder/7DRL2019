"use strict";

exports.readDouble = function (offset) {
  return function (buf) {
    return function() {
      return buf.readDoubleBE(offset);
    };
  };
};
