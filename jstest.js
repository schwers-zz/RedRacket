
// Testing javascript regexp times

var numtests = 20;

// Run some tests and give output
function test(re, stringbuilder, text, result, max, min) {
    max = max || 20;
    min = min || 10;
    var results = new Array();
    print("\n\n");
    for (var i = min; i <= max; i++) {
        var string = stringbuilder(i);
        var times = new Array();
        var expected = true;
        for (var j = 0; j < numtests; j++) {
            var t1 = (new Date()).getTime();
            expected &= (result == re.test(string));
            var t2 = (new Date()).getTime();
            var delta = t2 - t1;
            times.push(t2 - t1);
        }
        var temp = [string.length];
        mean_variance(times,temp);
        temp.push(expected);
        results.push(temp);
    }
    print_results(results, text);
}

function mean_variance(numbers, place){
    var length = numbers.length;
    var xs = 0; var x2s = 0; 
    for (var i = 0; i < length; i++) {
        var x = numbers[i];
        xs += x; x2s = ( x * x );
    }
    var mean = (xs / length);
    var vari = (x2s / length) - (mean * mean);
    place.push(mean);
    place.push(vari);
}

/* Expects an array of sub-array with four elements [sizemean,variance,passed?] */
function print_results(result_array, test_description){
    var line0 = doublequote("Size of Input:") + ";";
    var line1 = doublequote("Mean time:") + ";";
    var line2 = doublequote("Vari time:") + ";";
    var line3 = doublequote("Test passed?:") + ";";
    var length  = result_array.length;
    for (var i = 0; i < length; i++){
        var res = result_array[i];
        line0 += (doublequote(res[0]) + ";");
        line1 += (doublequote(res[1]) + ";");
        line2 += (doublequote(res[2]) + ";");
        line3 += (doublequote(passfail(res[3])) + ";");
    }
    print(doublequote("Test" + test_description) + "\n" +
          line0 + "\n" +
          line1 + "\n" +
          line2 + "\n" +
          line3);
}

function doublequote(val){ return "\"" + val + "\""; }
function passfail(bool){ if (bool) { return "Passed"; } { return "Failed"; } }


// build large strings by powers of two of the input string
function buildByTwos(str, pow) {
    if (pow <= 0) { return str;}
    else { return buildByTwos(str + str, pow - 1);}
}

// swap the first character with a random one
function swap(str) {
    var pos = Math.floor(Math.random() * (str.length - 1)) + 1;
    var chr1 = str.substring(0,1);
    var chr2 = str.substring(pos,pos + 1);
    var middle = str.substring(1,pos);
    var end = str.substring(pos + 1,str.length);
    return chr2 + middle + chr1 + end;
}

// make a random permuation of a string by swapping all
// the way down a string
function permute(str) {
    var res = str;
    var len = res.length;
    var temp = "";
    for (var i = 0; i < (len - 1); i++){
        temp = res.substring(0,i) + swap(res.substring(i,len));
        res = temp;
    }
    return res;
}


function buildPermutations(str,n){
    var res = [str];
    for (var i = 1; i < n; i++){
        res[i] = permute(str);
    }
    return res;
}

function insertRandom(str,ins){
    var r = (Math.random() * (str.length - 1)) + 1;
    return str.substring(0,r) + ins + str.substring(r+1,str.length);
}

function insertMiddle(str,ins){
    var len = str.length;
    var mid = Math.round(len / 2);
    return str.substring(0,mid) + ins + str.substring(mid, len);
}

function permutesThenInsert(str, ins, n){
    var res = buildPermutations(str,n);
    for (var i = 0; i < res.length; i++){
        res[i] = insertRandom(res[i], ins);
    }
    return res;
}

function insertToAll(strs,ins){
    for (var i = 0; i < strs.length; i++){
        strs[i] = insertMiddle(strs[i], ins);
    }
    return strs;
}

function beforeAndAfter(str,pre,post){
    return pre + str + post;
}

function windAll(strs,pre,post){
    for (var i = 0; i < strs.length; i++){
        strs[i] = beforeAndAfter(strs[i],pre,post);
    }
    return strs;
}

function permutesInMiddle(str,pre,post,n){
    var res = buildPermutations(str,n);
    for (var i = 0; i < res.length; i++){
        res[i] = beforeAndAfter(res[i],pre,post);
    }
    return res;
}

var re0 = /.*schwers.r@gmail.com.*$/;
var re1 = /^a*/;
var re2 = /\(a+\)/;
var re3 = /^www\.[a-z]+\.[a-z]{2,4}/;
var re4 = /[a-z0-9\_\%\+\-\.]+@[a-z0-9\.\_]+\.[a-z]{2,4}/;

function as(n){
    return  buildByTwos("a", n);
}

function aemailsa(n){
    var s = as(n);
    return s + "schwers.r@gmail.com" + s;
}

function parens(n){
    var s = as(n);
    return "(" + s + ")";
}

function urls(n){
    var s = buildByTwos("lambdafxxfxoisadf", n);
    return "www." + s + ".com";
}


function emails(n){
    var s = buildByTwos("this.is.a.test.right", n);
    return s + "@yup.com";
}

function runTests(){
    /* make sure the regexs are compiled */
    re0.compile; re1.compile; re2.compile; re3.compile; re4.compile;
    test(re0, aemailsa, " a*emaila*", true);
    test(re1, as, " a* only", true);
    test(re2, parens, " (a+)", true);
    test(re4, emails, " email validation", true);
    test(re3, urls, " url validation", true);
}






