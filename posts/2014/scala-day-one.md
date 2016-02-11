---
title: Scala Day One
date: 2014-04-09
description: The Castle on the Hill
tags: scala
---

The first day of Scala study covered syntax, the type system, and some language
features like type inference. Scala is presented as a *“bridge [between] two
paradigms”*, those being object-oriented programming and functional
programming. We’re saving the functional stuff for another day.

I did some preliminary setup before writing any Scala. It seems most developers
run a full-blown IDE like IntelliJ IDEA, NetBeans or Eclipse. Since I use Vim
for everything, I would likely be missing out on some powerful debug/refactor
tools.

I decided to first look for tools to aid Scala-development in Vim. A quick
search pulled up [‘My Vim setup for
Scala’](http://bleibinha.us/blog/2013/08/my-vim-setup-for-scala) by Stefan
Bleibinhaus, which made for a good starting point.

<div id="toc"></div>

1. [Things to find](#things-to-find)
  1. [The Scala API](#the-scala-api)
  2. [A comparison of Java and Scala](#a-comparison-of-java-and-scala)
  3. [A dicussion of val versus var](#a-discussion-of-val-versus-var)
2. [Tic Tac Toe](#tic-tac-toe)
  1. [Drawing the board](#drawing-the-board)
  2. [Checking end conditions](#checking-end-conditions)
  3. [Creating players](#creating-players)
  4. [The main game loop](#the-main-game-loop)
  5. [Starting the game](#starting-the-game)
3. [Thoughts](#thoughts)

### Things to find

#### The Scala API

Scala’s official website has all kinds of documentation including cheatsheets,
tutorials, a style guide, a glossary, and [the
API](http://www.scala-lang.org/documentation/api.html).

#### A comparison of Java and Scala

Finding a comparison of Java and Scala is no challenge. Programmers *love* to
compare languages, especially with regards to syntax and performance. The two
pages I found most interesting were [this article on
InfoQ](http://www.infoq.com/articles/scala-java-myths-facts) and [this question
on Stack
Overflow](http://stackoverflow.com/questions/13707809/java-8-and-scala).

#### A discussion of val versus var

Once again, Stack Overflow [pulls through for
us](http://stackoverflow.com/questions/1791408/what-is-the-difference-between-a-var-and-val-definition-in-scala).
The tl;dr version: `var` is mutable and can change at any time, whereas `val`
is immutable and can’t, though the object a `val` references can still change.

### Tic Tac Toe

> Implement a game of Tic Tac Toe

Before writing any code, I spent some time thinking about how I would approach
this problem. I intend for this little game to live in the CLI, so I’ll have to
find some way of drawing an ASCII game board. I’ll need to create the concept
of a turn since Tic Tac Toe is a turn-based game. I’ll need to create a couple
of players, and I need a way of checking whether or not a player has won.

#### Drawing the board

A TicTacToe board looks to me like a 3x3 matrix, so that’s what I used to
represent it. Creating a matrix and filling each element with something is a
one-liner in Scala. Printing an aesthetically pleasing game board from ASCII
characters was trial and error; we take each row of our matrix and interpolate
hyphens and pipes to use as dividers.

```scala
var gameBoard = List.fill[Char](3,3)(' ')

def makeRow(row: List[Char]) {
  var left = " " + row(0) + " |"
  var middle = " " + row(1) + " |"
  var right = " " + row(2) + " "
  println(left.concat(middle).concat(right))
}

def render() {
  clearScreen
  val divider = "---+---+---"
  makeRow(gameBoard(0))
  println(divider)
  makeRow(gameBoard(1))
  println(divider)
  makeRow(gameBoard(2))
}

def clearScreen = (1 to 30).foreach(_ => println("\n"))
```

To clear the screen after each turn, I defined a method called `clearScreen`
which just prints a bunch of new lines. It’s interesting that Scala allows us
to elegently do this as a one-liner.

#### Checking end conditions

An integral part of the game mechanics is checking whether a player has managed
to place their marker in three contiguous spaces, vertically, horizontally, or
diagonally. I don’t know if my approach was idiomatic for Scala, or if it was
particularly elegant, but at this stage I just wanted to get something working.

Scala provides a wide range of helper methods for working with collections. I
used the `forall` method to check if all elements in a given collection meet a
certain condition. Each check represents a possible winning combination, and I
chained the checks together with logical `or`operators.

```scala
def gameHasMetEndCondition(Player: Char) : Boolean = {
  if (gameBoard(0).forall(x => (x.equals(Player)))
      || gameBoard(1).forall(x => (x.equals(Player)))
      || gameBoard(2).forall(x => (x.equals(Player)))
      || List(gameBoard(0)(0), gameBoard(1)(0), gameBoard(2)(0)).forall(x => (x.equals(Player)))
      || List(gameBoard(0)(1), gameBoard(1)(1), gameBoard(2)(1)).forall(x => (x.equals(Player)))
      || List(gameBoard(2)(0), gameBoard(2)(1), gameBoard(2)(2)).forall(x => (x.equals(Player)))
      || List(gameBoard(0)(0), gameBoard(1)(1), gameBoard(2)(2)).forall(x => (x.equals(Player)))
      || List(gameBoard(2)(0), gameBoard(1)(1), gameBoard(0)(2)).forall(x => (x.equals(Player)))) return true
  return false
}

abstract class Result
case class GameResult(winner: Player) extends Result {
  println("The game has finished. ".concat(winner.name).concat(" wins!"))
}
case class Draw extends Result {
  println("It’s a tie. Everyone’s a winner.")
}
```

#### Creating players

The concept of a player suits the object-oriented paradigm well. We create a
`Player` class, and create two players, one for `X` and one for `O`.

```scala
class Player(playerMarker: Char) {
  val marker = playerMarker
  var name = ""
}

var playerOne = new Player('X')
var playerTwo = new Player('O')
```

#### The main game loop

Scala developers seem to favour recursion over loops, so the turn-system is
created with a method that calls itself. At the end of each turn, the
`turnsRemaining` value is decremented so we can detect the scenario when there
are no more possible moves, and the game is a draw.

I’m using the old modulus trick to decide who’s turn it is. I think hard-coding
values like this is acceptable since I’m not desigining the game to scale in
the future to support arbitrarily-sized boards or more than two players.

I take two values from standard input for the current player: the row, and the
column. I could extend this to support chess-like coordinates, *i.e.*, ‘A2’ or
‘C3’, but the system works fine as it is now. Once we have those two values, we
updated the cell at the given position in the matrix.

If we detect an end condition, we return early from the `doTurn` method.
Otherwise, call it again.

```scala
def doTurn(turnsRemaining: Int) : Result = {
  if (turnsRemaining == 0) return Draw()
  render
  var currentPlayer = if (turnsRemaining % 2 < 1) playerTwo else playerOne

  println((currentPlayer.name).concat(", make your move."))
  var row = readLine.toInt
  var column = readLine.toInt
  gameBoard = gameBoard.updated(row, gameBoard(row).updated(column, currentPlayer.marker))

  if (gameHasMetEndCondition(currentPlayer.marker)) return GameResult(currentPlayer)
  doTurn(turnsRemaining -1)
}
```

#### Starting the game

To wrap up, we have an imperative list of commands that get the game started.
We clear the screen, take the names of the players, and call the `doTurn`
method. I’m passing in `9` because that’s the number of cells on a TicTacToe
gameboard, and the `doTurn` method will stop running once there are no cells
left to occupy.

```scala
clearScreen
println("Welcome to TicTacToe. First player, what’s your name?")
playerOne.name = readLine
println("Second player, what’s your name?")
playerTwo.name = readLine
doTurn(9)
```

### Thoughts

So far, I have mixed feelings about the language. Some parts feel concise and
elegant, and other parts felt a bit awkward. I like that Scala allows for you
to do away with some notation in certain contexts, but I imagine other people
might rather all methods definitions look the same syntactically, everywhere.

Writing code for a compiler felt particularly awkward, because I couldn’t
always trace the code flow like I might in JavaScript. This *could* be a
blessing in disguise though, as I don’t think printing stuff to the screen is
always an ideal debugging technique.

I’m quite confident that Scala will start to make more sense once I come to
appreciate the functional side. If anything, Scala makes a good language for
preparing me for Erlang, Clojure and Haskell.

<a class="previous-post" href="/seven-languages/io-day-three">« Io: <i>The Parade and Other Strange Places</i></a>
<a class="next-post" href="/seven-languages/scala-day-two">Scala: <i>Clipping Bushes and Other New Tricks</i> »</a>
