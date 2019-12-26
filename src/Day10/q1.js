p0 = `.###
      #.##`


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

const {Elm} = require("./q1.compiled.js");

const app = Elm.Day10.Q1.init({ flags: puzzle });
app.ports.toJs.subscribe(data => console.log(data));
