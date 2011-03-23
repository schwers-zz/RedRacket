// Testing javascript regexp times


// Run some tests and give output
function test(re, strs, text) {
    print("Running test for" + text);
    var time = 0;
    var num = strs.length;
    for (var i = 0; i < num; i++) {
        var t1 = dateNow();
        re.test(strs[i]);
        var t2 = dateNow();
        var delta = t2 - t1;
        time = time + delta;
        print("\t" + i + ": " + delta + "\n")
    }
    print("\tAverage: " + (time / num) + "\n")
}


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

function permutesThenInsert(str, ins, n){
    var res = buildPermutations(str,n);
    for (var i = 0; i < res.length; i++){
        res[i] = insertRandom(res[i], ins);
    }
    return res;
}

function insertToAll(strs,ins){
    for (var i = 0; i < strs.length; i++){
        strs[i] = insertRandom(strs[i], ins);
    }
    return strs;

}

function beforeAndAfter(str,pre,post){
    return pre + str + post;
}

function permutesInMiddle(str,pre,post,n){
    var res = buildPermutations(str,n);
    for (var i = 0; i < res.length; i++){
        res[i] = beforeAndAfter(res[i],pre,post);
    }
    return res;
}

var re0 = /.*schwers.r@gmail.com.*$/;
var re1 = /^a*$/;
var re2 = /^www\.[a-z]+\.[a-z]{2,4}$/;
var re3 = /^[a-z0-9\_\%\+\-\.]+@[a-z0-9\.\_]+\.[a-z]{2,4}$/;

var smallas = buildByTwos("a", 20);
var as = buildByTwos("a", 26);
var email = "schwers.r@gmail.com";
var smemail = smallas + email + smallas;
var data0 = [smemail, smemail, smemail, smemail, smemail, smemail];
var aemaila = as + emailo + as;
var data01 = [aemaila, aemaila, aemaila, aemaila, aemaila];
var data1 = [as,as,as,as,as,as,as,as,as,as];
var zas = "Z" + as;
var data2 = [zas,zas,zas,zas];
var asz = as + "Z";
var data3 = [asz, asz, asz, asz, asz];
var webfiller = "www." + buildByTwos("lambdafxxfxoiasdf", 10) ".com";
var data4 = [webfiller, webfiller, webfiller, webfiller, webfiller];
var email = buildByTwos("this.is.a.test.right.", 15) + "@yup.";
var data5 = [email + "com", email + "test", email + "uk"];


function runTests(){
    test(re0, data0, " Email in the middle of a string of a");
    test(re0, data01, " Email in the middle of a string of a");
    test(re1, data1, " A's only, should match");
    test(re1, data2, " A's only, should fail fast");
    test(re1, data3, " A's only, should fail slow");
    test(re3, emails, " Basic email validation");
    test(re2, data4, " Random website");
}






