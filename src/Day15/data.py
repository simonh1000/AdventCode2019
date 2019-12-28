input = """10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL"""

# process input


def mkRecipes(lines):
    res = {}
    for line in lines:
        (result, quantity, ingredients) = mkRecipe(line)
        res[result] = (quantity, ingredients)
    return res


def mkRecipe(line):
    [ingredients, result] = str.split(line, " => ")
    p1 = str.split(ingredients, ", ")
    recipe = []
    for r in p1:
        recipe.append(mkIngredient(r))

    [q, res] = mkIngredient(result)

    return (res, q, recipe)


def mkIngredient(s):
    [q, n] = str.split(s, " ")
    return [int(q), n]


data = mkRecipes(str.split(input,  "\n"))

# ordering

ordering = {}


def orderRecipe(lst):
    tmp = 0

        1 + max(for l is lst:)


def orderItem(item):
    if ordering[item[1]]:
        ordering[item]
    elsif item[1] == 'ORE':
        0
    else:
        1 + orderRecipe(data[item[1]])

# def apply(amtNeeded, recipe):
#     [quant, ingredients] = recipe

#     # round up
#     if amtNeeded % quant == 0:
#         units = amtNeeded // quant
#     else:
#         units = 1 + amtNeeded // quant

#     tot = 0
#     for [q, n] in ingredients:
#         if n == 'ORE':
#             tot += q
#         else:
#             tot += apply(q, data[n])

#     print(units, ingredients, tot)
#     return(units * tot)


# def recursor(ingedients):
#     return ingredients


print(data)
print("I need: ", data['FUEL'])
# print("*: ", apply(1, data['D']))
# print("FUEL: ", apply(1, data['FUEL']))
