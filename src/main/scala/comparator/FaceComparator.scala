package comparator

import java.awt.event.{MouseEvent, MouseListener}
import java.io.File
import javax.swing._
import javax.swing.event.{ChangeEvent, ChangeListener}

import scalismo.faces.color.RGBA
import scalismo.faces.gui.{GUIBlock, GUIFrame, ImagePanel}
import scalismo.faces.gui.GUIBlock._
import scalismo.faces.io.PixelImageIO
import scalismo.faces.image.{BufferedImageConverter, PixelImage}
import scalismo.faces.image.BufferedImageConverter._
import scalismo.faces.image.PixelImage


object FaceComparator extends App {
  val path = "/export/faces/projects/pami-ppm2017/experiments/fit-multipie-recognition/resultsNotForPublishing/bfm"

  def getSubDirs(root: String):List[File] = {
    val f = new File(root)
    if( f. exists() && f.isDirectory ){
      f.listFiles.filter(_.isDirectory).toList
    }else{
      println("getSubDirs: none existant dir/ no directory")
      List[File]()
    }
  }

  val subDirs = getSubDirs(path)
  println("Found "+subDirs.length+" Subdirectories")



  def getImages(dir: File): (PixelImage[RGBA],PixelImage[RGBA]) ={
    val targetName = "/target.png"
    val bestFitName = "/fitter-best.png"

    val targetFile = new File(dir.getAbsolutePath+targetName) //ugly
    val targetPI = PixelImageIO.read(targetFile)(ConverterRGBA).get

    val bestFitFile = new File(dir.getAbsolutePath+bestFitName) //ugly
    val bestFitPI = PixelImageIO.read(bestFitFile)(ConverterRGBA).get

    (targetPI, bestFitPI)
  }



  //starting value
  //ugly hacking, could be made nicer...  but I like hackbraten
  val dirsIterator = Iterator.continually(subDirs).flatten
  val startDir = dirsIterator.next()

  val (targetPI, bestFitPI) = getImages(startDir)
  val targetPL = ImagePanel(targetPI)
  val bestFitPL = ImagePanel(bestFitPI)

  //image blending function for blended image
  val opacity = 0.3
  def blendWithOpacity(target:RGBA, fit:RGBA):RGBA = {
    val cFit = fit+RGBA(1,0,0) //color the fit red
    fit.a match{
      case 0.0 => target
      case 1.0 => target*(1-opacity)+cFit*opacity
    }
  }
  def blendWithAlpha(target:RGBA, fit:RGBA):RGBA = {
    fit.a match{
      case 0.0 => target
      case 1.0 => fit
    }
  }

  val opaPI = targetPI.zip(bestFitPI).map {case (a,b) => blendWithOpacity(a,b) }
  val opaPL = ImagePanel(opaPI)


  case class ClickImagePanel[A](var a:PixelImage[A],var b:PixelImage[A])(implicit conv: BufferedImageConverter[A]) extends ImagePanel(a){
    var isA = true
    def swapImages(): Unit ={
      if(isA){
        super.updateImage(b)
      }else{
        super.updateImage(a)
      }
      isA = !isA
    }

    def updateImages(nA:PixelImage[A], nB:PixelImage[A]): Unit ={
      this.a = nA
      this.b = nB
      super.updateImage(this.a)
    }

    super.addMouseListener(new MouseListener {
      override def mouseExited(e: MouseEvent): Unit = {}
      override def mouseClicked(e: MouseEvent): Unit = {
        swapImages()
      }
      override def mouseEntered(e: MouseEvent): Unit = {}
      override def mousePressed(e: MouseEvent): Unit = {}
      override def mouseReleased(e: MouseEvent): Unit = {}
    })
  }


  val combinedPI = targetPI.zip(bestFitPI).map {case (a,b) => blendWithAlpha(a,b) }
  val combinedPL = ClickImagePanel(combinedPI, targetPI)

  //buttons and label
  def updateGUI(dir: File): Unit ={
    val (target, fit) = getImages(dir)
    targetPL.updateImage(target)
    bestFitPL.updateImage(fit)
    val opa = target.zip(fit).map {case (a,b) => blendWithOpacity(a,b) }
    opaPL.updateImage(opa)
    val comb = target.zip(fit).map {case (a,b) => blendWithAlpha(a,b) }
    combinedPL.updateImages(comb, target)

    subjectLabel.setText(dir.getName)
  }

  val nextButton = GUIBlock.button("next", {
    updateGUI(dirsIterator.next())
  })
  val prevButton = GUIBlock.button("previous", {
    //TODO extend iterator to make this work
  })
  val subjectLabel = GUIBlock.label(startDir.getName)

  val imageShelf = GUIBlock.shelf(targetPL, bestFitPL, opaPL, combinedPL)
  val controlShelf = GUIBlock.shelf(prevButton, GUIBlock.horizontalSeparator, subjectLabel, GUIBlock.horizontalSeparator, nextButton)

  val guiFrame: GUIFrame = GUIBlock.stack(imageShelf, controlShelf).displayIn("Face Comparator")



}
