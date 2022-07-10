var _ = require('lodash');
var assert = require('assert');
var fs = require('fs');

var babyparse = require('babyparse');

function readCSV(filename){
  return babyparse.parse(fs.readFileSync(filename, 'utf8'), {
    header: true,
    keepEmptyRows:false,
    skipEmptyLines: true
  }).data;
};

function readJSON(filename){
  return require(filename);
};

function writeCSV(jsonCSV, filename){
  fs.writeFileSync(filename, babyparse.unparse(jsonCSV) + '\n');
}

function appendCSV(jsonCSV, filename){
  fs.appendFileSync(filename, babyparse.unparse(jsonCSV) + '\n');
}

var writeERP = function(erp, labels, filename, fixed) {
  var data = _.filter(erp.support().map(
   function(v) {
     var prob = erp.score(v);
     if (prob > 0.0){
      if(v.slice(-1) === ".")
        out = butLast(v);
      else if (v.slice(-1) === "?")
        out = butLast(v).split("Is")[1].toLowerCase();
      else 
        out = v
      return labels.concat([out, String(prob.toFixed(fixed))]);

    } else {
      return [];
    }
  }
  ), function(v) {return v.length > 0;});
  appendCSV(data, filename);
};

// Note this is highly specific to a single type of erp
var bayesianErpWriter = function(erp, filePrefix) {
  var predictiveFile = fs.openSync(filePrefix + "Predictives.csv", 'w');
  fs.writeSync(predictiveFile, ["stim", "item1", "item2", "value",
				"alpha_A", "alpha_Q", "w", "modelType",
				"prediction", "posteriorProb"] + '\n');

  var paramFile = fs.openSync(filePrefix + "Params.csv", 'w');
  fs.writeSync(paramFile, ["alpha_A", "alpha_Q", "w", "modelType",
			   "logLikelihood", "posteriorProb"] + '\n');

  var supp = erp.support();
  supp.forEach(function(s) {
    supportWriter(s.predictive, erp.score(s), predictiveFile);
    supportWriter(s.params, erp.score(s), paramFile);
  });
  fs.closeSync(predictiveFile);
  fs.closeSync(paramFile);
  console.log('writing complete.');
};

var supportWriter = function(s, p, handle) {
  var sLst = _.toPairs(s);
  var l = sLst.length;

  for (var i = 0; i < l; i++) {
    fs.writeSync(handle, sLst[i].join(',')+','+p+'\n');
  }
}

var capitalize = function(string) {
    return string.charAt(0).toUpperCase() + string.slice(1);
}


var flatten = function(xs){
  if (xs.length == 0) {
    return [];
  } else {
    return xs[0].concat(flatten(xs.slice(1)));
  }
};

function setsEqual(a1, a2){
  var s1 = a1.slice().sort();
  var s2 = a2.slice().sort();
  return JSON.stringify(s1) === JSON.stringify(s2);
}

function arraysEqual(a1, a2){
  return JSON.stringify(a1) === JSON.stringify(a2);
}

function powerset(set) {
  if (set.length == 0)
    return [[]];
  else {
    var rest = powerset(set.slice(1));
    return rest.map(
      function(element) {
        return [set[0]].concat(element);
      }).concat(rest);
  }
}

// Sometimes you just need all possible combination of true and false
var TFCartesianProd = function(n) {
  var result = [];
  _.map(_.range(n), function(i){
    result.push(['true', 'false']);
  });
  return cartesianProductOf(result);
};

var permute = function (old_input) {
  var input = old_input.slice();
  var permArr = [];
  var usedChars = [];
  var doPerm = function() {
    if (input.length == 0) {
      permArr.push(usedChars.slice());
    }
    _.range(input.length).map(
      function(i) {
        var ch = input.splice(i, 1)[0];
        usedChars.push(ch);
        doPerm();
        usedChars.pop();
        input.splice(i, 0, ch);
      });
  };
  doPerm();
  return permArr;
};

var cartesianProductOf = function(listOfLists) {
  return _.reduce(listOfLists, function(a, b) {
    return _.flatten(_.map(a, function(x) {
      return _.map(b, function(y) {
        return x.concat([y]);
      });
    }), true);
  }, [ [] ]);
};


var normalizeArray = function(xs) {
  var Z = sum(xs);
  return xs.map(function(x) {
    return x / Z;
  });
};


var butLast = function(xs){
  return xs.slice(0, xs.length-1);
};

function _logsumexp(a) {
  var m = Math.max.apply(null, a);
  var sum = 0;
  for (var i = 0; i < a.length; ++i) {
    sum += (a[i] === -Infinity ? 0 : Math.exp(a[i] - m));
  }
  return m + Math.log(sum);
}

var printERP = function(erp) {
  erp.support().map(
    function(v) {
      var prob = Math.exp(erp.score(v));
      if (prob > 0.0){
        console.log({val: v, prob: prob});
      }
    }
  );
};

var sortWithIndices = function(toSort) {
  for (var i = 0; i < toSort.length; i++) {
    toSort[i] = [toSort[i], i];
  }
  toSort.sort(function(left, right) {
    return left[0] < right[0] ? -1 : 1;
  });
  toSort.sortIndices = [];
  for (var j = 0; j < toSort.length; j++) {
    toSort.sortIndices.push(toSort[j][1]);
    toSort[j] = toSort[j][0];
  }
  return toSort;
};

// Used these to speed stuff up...

//var answererModelOutput = readCSV('./spatialAnswererOutput_HierarchicalVersion.csv');
// var questionerModelOutput = require('../experiment3/spatialQuestionerOutput_HierarchicalVersion.json');

// function getQuestionerOutput(type, goal, gridState) {
//   var goalString = _.includes(goal, '1') ? 'columns': 'rows';

//   return _.filter(questionerModelOutput, {
//     questionerType : type,
//     gridState: JSON.stringify(gridState),
//     goal: goalString
//   });
// }

// function alreadyInOutput(world) {
//   return _.find(answererModelOutput, {world: world}) ? true : false;
// }

module.exports = {
  arraysEqual: arraysEqual,
  setsEqual: setsEqual,
  powerset: powerset,
  flatten: flatten,
  permute: permute,
  cartesianProductOf: cartesianProductOf,
  TFCartesianProd : TFCartesianProd,
  butLast: butLast,
  printERP: printERP,
  readCSV: readCSV,
  readJSON: readJSON,
  writeCSV: writeCSV,
  appendCSV: appendCSV,
  writeERP: writeERP,
  bayesianErpWriter: bayesianErpWriter,
  normalizeArray: normalizeArray,
  capitalize: capitalize
};
