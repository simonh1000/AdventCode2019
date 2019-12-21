const t1 =
`COM)B
B)C`

const inp =
`COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L`

const puzzle = require('./data.js');
// 122782

const {Elm} = require("./q1.compiled.js");

const app = Elm.Day06.Q1.init({ flags: puzzle });
app.ports.toJs.subscribe(data => console.log(data));

