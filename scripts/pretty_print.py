from nltk.tree import Tree, TreePrettyPrinter

tree = Tree.fromstring(input().strip())
print(TreePrettyPrinter(tree).text())
