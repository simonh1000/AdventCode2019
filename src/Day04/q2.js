
const {Elm} = require("./q2.compiled.js");

const app = Elm.Day04.Q2.init({
    flags: "null"
});
app.ports.toJs.subscribe(data => {
    console.log(data);
    console.log(data.length);
});

//part1 = 4023471