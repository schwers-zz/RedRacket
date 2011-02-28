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

var re0 = /^[a-z]*schwers.r@gmail.com[a-z]*$/;
var re1 = /^a*$/;
var re2 = /^[a-z]*palindromeemordnilap[a-z]*$/;
var re3 = /^www\.[a-z]+\.com/;


var data0 = permutesThenInsert("asnvoiwefg", "schwers.r@gmail.com", 10);
var as = buildByTwos("a", 26);
var data1 = [as,as,as,as,as,as,as,as,as,as];
var data2 = insertToAll(data1, "qed");
var data3 = permutesThenInsert("asoqwxzc", "palindromeemordnilap", 10);
var webfiller = buildByTwos("lambdafxxfxoiasdf", 3);
var data4 = permutesInMiddle(webfiller, "www.", ".com", 10);

function runTests(){
    test(re0, data0, "Email in the middle of random string");
    test(re1, data1, "A's only, should match");
    test(re1, data2, "A's only, should fail");
    test(re2, data3, "Filler with a palindrome in the middle");
    test(re3, data4, "Random website");
}






