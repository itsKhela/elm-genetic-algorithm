Elm.Native.Randoms = {};

Elm.Native.Randoms.make = function(localRuntime) {

  localRuntime.Native = localRuntime.Native || {};


  localRuntime.Native.Randoms = localRuntime.Native.Randoms || {};

  if (localRuntime.Native.Randoms.values) {
    return localRuntime.Native.Randoms.values;
  }

  var Result = Elm.Result.make(localRuntime);

  return localRuntime.Native.Randoms.values = {
    getFloat: (Math.random() * 100000) + 1
  };

};
