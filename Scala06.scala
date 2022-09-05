import scala.io.StdIn._

object CeaserCipher {

    def cipher( text:String, my_function:(Char,Int) => Char, shift:Int ) : String = text.map (my_function ( _, shift ))   //pass the my_function to the map

    def encryption ( character:Char, shift:Int ) : Char = character match
        case x if !x.isLetter => x
        case x if x.isUpper => (( x-65 + shift ) %26 +65).toChar            //Z=90  25  30  4  69  E
        case x if x.isLower => (( x-97 + shift ) %26 +97).toChar

    def decryption ( character:Char, shift:Int ) : Char = character match
        case x if !x.isLetter => x
        case x if x.isUpper => (( x-65 + 26 - shift ) % 26 + 65).toChar     //Z=90  25  51  46  20  85  U
        case x if x.isLower => (( x-97 + 26 - shift ) % 26 + 97).toChar

    def main( args: Array[String] ) = {

        print("Enter a text : \n")
        var text = readLine()

        print("Key value to encrypt : \n")
        var key = readInt()

        if ( key<0 || key>25 ){
            println("Key value is out of range")
            sys.exit()
        }

        val encrpt = cipher( text, encryption, key )
        println("Encrypted String :   " + encrpt)

        val decrpt = cipher( encrpt, decryption, key )
        println("Decrypted String :   " + decrpt)

    }    
}

