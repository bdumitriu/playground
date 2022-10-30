let currentDate = new Date();
let timeString = [];
let hours = currentDate.getHours();
if (hours == 0) {
    timeString.push("It's midnight");
} else if (hours < 12) {
    timeString.push("It's ", hours % 12, " in the morning");
} else if (hours == 12) {
    timeString.push("It's noon");
} else if (hours < 19) {
    timeString.push("It's ", hours % 12, " in the afternoon");
} else {
    timeString.push("It's ", hours % 12, " in the evening");
}
console.log(timeString.join(""));
console.log("foo");