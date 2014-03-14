package se.jt

import java.io.File
import se.jt.frame.Compass._

class FileChooser(n:String, path:File) 
extends BorderPiece(
		n, 
		Map(
			NORTH -> new FileInputPiece("file-input"),
			CENTER -> new TablePiece("files", Array(), List[String]())
		)
) {
	
}