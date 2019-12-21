const t1 =
`COM)B
B)C`

const t2 =
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

const t3 =
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
 K)L
 K)YOU
 I)SAN`
const puzzle = require('./data.js');
// 122782

const {Elm} = require("./q2.compiled.js");

const app = Elm.Day06.Q2.init({ flags: puzzle });
app.ports.toJs.subscribe(data => console.log(data));

