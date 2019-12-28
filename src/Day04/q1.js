
const {Elm} = require("./q1.compiled.js");

const app = Elm.Day04.Q1.init({
    flags: "null"
});
app.ports.toJs.subscribe(data => console.log(data));

//part1 = 4023471