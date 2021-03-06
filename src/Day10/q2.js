p0 = `.#.
      ###`



p1 = `.#..#
      .....
      #####
      ....#
      ...##`

p2 = `......#.#.
      #..#.#....
      ..#######.
      .#.#.###..
      .#..#.....
      ..#....#.#
      #..#....#.
      .##.#..###
      ##...#..#.
      .#....####`

p3 = `#.#...#.#.
      .###....#.
      .#....#...
      ##.#.#.#.#
      ....#.#.#.
      .##..###.#
      ..#...##..
      ..##....##
      ......#...
      .####.###.`

p5 = `.#..##.###...#######
      ##.############..##.
      .#.######.########.#
      .###.#######.####.#.
      #####.##.#.##.###.##
      ..#####..#.#########
      ####################
      #.####....###.#.#.##
      ##.#################
      #####.##.###..####..
      ..######..##.#######
      ####.##.####...##..#
      .#####..#.######.###
      ##...#.##########...
      #.##########.#######
      .####.#.###.###.#.##
      ....##.##.###..#####
      .#.#.###########.###
      #.#.#.#####.####.###
      ###.##.####.##.#..##`

puzzle = `.###.#...#.#.##.#.####..
          .#....#####...#.######..
          #.#.###.###.#.....#.####
          ##.###..##..####.#.####.
          ###########.#######.##.#
          ##########.#########.##.
          .#.##.########.##...###.
          ###.#.##.#####.#.###.###
          ##.#####.##..###.#.##.#.
          .#.#.#####.####.#..#####
          .###.#####.#..#..##.#.##
          ########.##.#...########
          .####..##..#.###.###.#.#
          ....######.##.#.######.#
          ###.####.######.#....###
          ############.#.#.##.####
          ##...##..####.####.#..##
          .###.#########.###..#.##
          #.##.#.#...##...#####..#
          ##.#..###############.##
          ##.###.#####.##.######..
          ##.#####.#.#.##..#######
          ...#######.######...####
          #....#.#.#.####.#.#.#.##`

const {Elm} = require("./q2.compiled.js");

const app = Elm.Day10.Q2.init({ flags: puzzle });
app.ports.toJs.subscribe(data => console.log(data));

