"use strict";

// Note to reader: Add your solutions to this file

exports.volumeFn = function(w, h, l) {
  return (w * l * h);
};

exports.volumeArrow = w => h => l =>
  w * h * l;

exports.addComplex = a => b => {
  return {
    real: a.real + b.real,
    imag: a.imag + b.imag
  };
};

exports.cumulativeSumsComplex = arr => {
  let sum = { real: 0, imag: 0 };
  let sums = [];
  arr.forEach(x => {
    sum = {
      real: sum.real + x.real,
      imag: sum.imag + x.imag
    };
    sums.push(sum);
  });
  return sums;
};

exports.negateComplex = c => {
  return { real: -c.real, imag: -c.imag };
};

exports.quadraticRootsImpl = pair => q => {
  let a = { real: (-q.b) / (2 * q.a), imag: 0.0 };
  let b = { real: 0.0, imag: 0.0 };
  let sq_disc = q.b * q.b - 4 * q.a * q.c;
  if (sq_disc < 0.0) {
    b = { real: 0.0, imag: (Math.sqrt(-sq_disc)) / (2 * q.a) };
  } else {
    b = { real: (Math.sqrt(sq_disc) / (2 * q.a)), imag: 0.0 };
  }

  let pr = this.addComplex(a)(b);
  let nr = this.addComplex(a)(this.negateComplex(b));
  return pair(pr)(nr);
};

exports.valuesOfMapImpl = m => {
  let m = new Map(m);
  let s = new Set();
  for (let value of m.values()) {
    s.add(value);
  }
  return Array.from(s);
};
