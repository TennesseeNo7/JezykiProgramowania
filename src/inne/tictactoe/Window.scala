package inne.tictactoe

import java.awt.Color

import javax.swing.{ImageIcon, JFrame, JLabel}

class Window {

  val frame = new JFrame()

  val fields: Seq[Seq[JLabel]] = Seq(fill(3), fill(3), fill(3))

  initialize()

  def initialize() {

    frame.setBounds(100, 100, 317, 347)
    frame.setVisible(true)
    frame.setResizable(false)
    frame.setLocationRelativeTo(null)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setLayout(null)
    frame.getContentPane.setBackground(new Color(34, 34, 34))

    apply((e: JLabel) => e.setIcon(new ImageIcon("C:\\Users\\szymo\\Desktop\\empty.png")))
    apply((e: JLabel) => e.setSize(90, 90))
    apply((e: JLabel) => frame.getContentPane.add(e))
    var x, y = 0
    apply((e: JLabel) => {
      e.setLocation(10 + 100*x, 10 + 100*y)
      if(x > 1) { x = 0; y += 1 } else x += 1
    })

    val background = new JLabel()
    background.setIcon(new ImageIcon("C:\\Users\\szymo\\Desktop\\background.png"))
    background.setBounds(0, 0, 317, 347)
    frame.getContentPane.add(background)

  }

  private def apply(f: JLabel => Unit): Unit = {
    fields.foreach((r: Seq[JLabel]) => r.foreach(f))
  }

  private def fill(n: Int, out: Seq[JLabel] = Seq()): Seq[JLabel] = n match {
    case 0 => out
    case _ => fill(n-1, new JLabel +: out)
  }

}
