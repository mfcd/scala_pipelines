package ExampleDsl


class Snake(val x: Int, val y: Int) {

  def moves(direction: Direction) = {
    direction match {
      case Up(m) => new Snake(this.x, this.y + m)
      case Down(m) => new Snake(this.x, this.y - m)
    }
  }

  def andThen(direction: Direction) = moves(direction)

  def getsPowerUp = new PowerUpSnake(this)
  def andKeepsPowerUp = getsPowerUp

}



trait CanEat {

  // we could to plan for some more action here but the real focus is the DSL design
  def eats(otherSnake: Snake) =
    if (this.hashCode == otherSnake.hashCode) throw new Exception("Can't eat yourself")
    else this

}


class PowerUpSnake(s: Snake) extends Snake(s.x, s.y) with CanEat {

  def losesPowerUp = new Snake(x, y)
  override def moves(d: Direction) = s moves d andKeepsPowerUp

}




