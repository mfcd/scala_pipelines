package ExampleDsl

abstract class Direction
case class Up(steps: Int) extends Direction
case class Down(steps: Int) extends Direction

