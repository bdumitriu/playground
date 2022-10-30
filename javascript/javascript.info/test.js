"use strict"
console.log("Hello world!");
let admin;
let name;
name = "John";
admin = name;
console.log(admin);
let planetName = "Earth";
let currentUserName = "bogdan";
let number = Infinity;
console.log(number);
const PRIVATE_KEY = 123456789123456789123456789n;
console.log(PRIVATE_KEY);
let str = "Hello";
let str2 = 'Single quotes are ok too';
// Backticks needed to evaluate expressions in a string as below
let phrase = `can embed another string ${str} or expression ${1+2}`;
console.log(phrase);
 // null is not a “reference to a non-existing object” or a “null pointer”.
 // It’s just a special value which represents “nothing”, “empty” or “value unknown”.
let nothingVar = null;
console.log(typeof null);
console.log(typeof console);
console.log(typeof Math);
console.log(typeof `{1+2}`);
console.log(typeof Symbol(1));
// [1].forEach((i) = console.log(i));

function sayHi() {
    console.log("foo");
}
console.log(sayHi);

