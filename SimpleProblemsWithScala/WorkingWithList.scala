object WorkigWithList{
	

	def main(args: Array[String]): Unit = {
      println("lets start solving problems")	  
      
      val problems : p01to29 = new p01to29()

      val inputList = List(1,2,3,4,5)	
      
      //output of the problem 1 
      val result = problems.getLast(List(1,2,3,4,5))
      println(result) 

      //output of the problem 2
      println(problems.getSecondLast(inputList))
     
      //output of the problem 3 
      println(problems.getnth(2,inputList))
      
      //print out put for problem 7
      println("Problem 7:")
      println(problems.flatten(List(List(1,2,3,4),7,"asdfdsf",List(1,2,3))))
      
      //problem 8: 
      println("Problem 8: Eliminate consecutive duplicates of list elements.")
      println(problems.compress(List(1,2,3,4,5,6)))
      println(problems.compress(List(1,1)))
      println(problems.compress(List("a","b")))
      println(problems.compress(List(1,3,3,4,4,5,5,6)))
      println(problems.compress(List(1)))
      println(problems.compress(List(1,11,11,1,1,11,1,1,1,1,1,1,1,1,1,1,1,1)))
      
      //Problem 09:
      println("P09: Pack consecutive duplicates of list elements into sublists.")
	  println(problems.pack(List(1,2,3,4,5,6)))
      println(problems.pack(List(1,1)))
      println(problems.pack(List("a","b")))
      println(problems.pack(List(1,3,3,4,4,5,5,6)))
      println(problems.pack(List(1)))
      println(problems.pack(List(1,11,11,1,1,11,1,1,1,1,1,1,1,1,1,1,1,1)))
      

      //Problem 10: 
      println("P10: run-length encoding")
      println(problems.encode(List(1,2,3,4,5,6)))
      println(problems.encode(List(1,1)))
      println(problems.encode(List("a","b")))
      println(problems.encode(List(1,3,3,4,4,5,5,6)))
      println(problems.encode(List(1)))
      println(problems.encode(List(1,11,11,1,1,11,1,1,1,1,1,1,1,1,1,1,1,1)))
      
      //Problem 11:
      println("P11: modified run-length encoding")
      println(problems.modifiedEncode(List(1,2,3,4,5,6)))
      println(problems.modifiedEncode(List(1,1)))
      println(problems.modifiedEncode(List("a","b")))
      println(problems.modifiedEncode(List(1,3,3,4,4,5,5,6)))
      println(problems.modifiedEncode(List(1)))
      println(problems.modifiedEncode(List(1,11,11,1,1,11,1,1,1,1,1,1,1,1,1,1,1,1)))
      
      //problem 12;
      println("P12 decode")
      println(problems.decode(List(("a",2),("v",3))))

      //problem 13:
      println("P13 Run-length encoding of a list (direct solution).")
      println(problems.directEncode(List(1,2,3,4,5,6)))
      println(problems.directEncode(List(1,1)))
      println(problems.directEncode(List("a","b")))
      println(problems.directEncode(List(1,3,3,4,4,5,5,6)))
      println(problems.directEncode(List(1)))
      println(problems.directEncode(List(1,11,11,1,1,11,1,1,1,1,1,1,1,1,1,1,1,1)))
	}
	
}

class p01to29{
   //p01 get last element of List 
   def getLast(input:List[_]) = {
       input.last
   }

   //p02 Find the last but one element of a list.
   def getSecondLast(input:List[_]) = {
   	val size = input.size
   	if(size > 1){
   		input(size - 2)
   	}
   	else{
   		0
   	}
   }

   //p03 get nth element of list 
   def getnth(n:Int , input:List[_]) = input(n)

   //p04 find number of element in list 
   def getSize(input:List[_]) : Int = input.size

   //P05 reverse List 
   def getReverse(input : List[_]) : List[_] = input.reverse

   //P06 Find out whether a list is a palindrome 
   def isPalindrome(input : List[_]) : Boolean = {
    //get reversed list 
    val reverseList = getReverse(input.reverse) 

    // checks if original list and reversed list are equals are not 
    input.equals(reverseList)
   }

   //P07 flatten a nested list structure 
   def flatten(input:Any) : List[_] = input match {
   	case Nil => Nil
   	case head :: tail =>  flatten(head) ::: flatten(tail)
   	case any => List(any) 
   }
   
   //P08 compress list
   def compress(input : List[_]) : List[_] = input match {
   	case Nil => Nil
   	case a :: b :: tail if(a == b) => compress(a :: tail)
   	case a :: b :: tail => List(a) ::: compress(b :: tail)
    case a :: Nil => List(a)

   }
   
   //P09 Pack 
   def pack(input : List[_]) : List[List[_]] = input match {
   	case Nil => Nil
   	case a :: Nil => List(List(a))
   	case a :: b :: tail if( a != b) => List(List(a)) ::: pack(b :: tail)
   	case a :: b :: tail => pack(tail) match {
   		case Nil =>  List(List(a,b))
   		case c :: d if(c.head == b) => List(List(a,b) ::: c) ::: d
   	    case c  => List(List(a,b)) ::: c
   	}
   	 
   }
   
   //P10 List encoding with map   
    def encode(input : List[_]) : List[(_,_)] = {
    	//case Nil => Nil
        pack(input) map{a:List[_] => (a head , a size)}
    	
    }

    //P11 Modified run Encoding
    def modifiedEncode (input : List[_]) : List[_] = {
    	pack(input) map{a:List[_]=> a size match {
    									case 1 => a head
    									case _ => (a head, a size)
    	    						}
        }
    } 
    
    //p11 Decode a run-length encoded list
    def decode( input : List[(_,Int)]) : List[_] = { 
       input flatMap{ a:(_,Int) => a match {
       	case (_,_)  => (1 to a._2) map( _ => a._1 ) 
        }
       }       
    }

    //P12 Run-length encoding of a list (direct solution).
    def directEncode( input : List[_]) : List[(_,Int)] = {
    	 input match {
    	 	case Nil => Nil
    	 	case  a :: Nil => List((a,1))
    	 	case a :: b => directEncode(b) match {
    	 		case Nil => Nil 
    	 		case c::d if(a == c._1) => (a,c._2 + 1 ) :: d
    	 		case c => (a,1) :: c 
    	 	}
    	 	 
    	 }
    	 
    	 
    }
        


}