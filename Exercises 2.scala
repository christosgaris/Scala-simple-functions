
object Exercises {
  	

	// 1, 12 - from
	/* exam-2015.pdf - Exercise 1 */
	/* 19 Dec 2016 – IT712A161219.pdf - Exercise 2 */
    def from(n: Int, m: Int): List[Int] = {
    	if (n > m) Nil
    	else n :: from(n+1, m)
	}
	//Exercises.from(3,7)
	//List(3,4,5,6,7)


	// 2, 13 - pascalRow
	/* exam-2015.pdf - Exercise 2 */
	/* 19 Dec 2016 – IT712A161219.pdf - Exercise 3 */
	def pascalNewRow(l: List[Int]): List[Int] = l match {
		case Nil => Nil
		case a :: Nil => a :: Nil // a will be 1
		case a :: b :: t => if (a == 1) 1 :: (a+b) :: pascalNewRow(b :: t)
							else (a+b) :: pascalNewRow(b :: t)
	}
	//Exercises.pascalNewRow(List(1,3,3,1))
	//List(1,4,6,4,1)


	// 3, 11 - interleave
	/* exam-2015.pdf - Exercise 3 */
	/* 19 Dec 2016 – IT712A161219.pdf - Exercise 1 */
	def interleave(s1: Stream[Int], s2: Stream[Int]): Stream[Int] = {
		s1.head #:: s2.head #:: interleave(s1.tail, s2.tail)
	}
	//Exercises.interleave(Stream(1,2,3,4),Stream(10,20,30,40)).take(8).toList
	//List(1, 10, 2, 20, 3, 30, 4, 40)


	// 4 - append
	/* exam-2015.pdf - Exercise 4 */
	def append(l1: List[Int], l2: List[Int]): List[Int] = {
		if (l1 != Nil) l1.head :: append(l1.tail, l2)
		else if ((l1 == Nil) && (l2 != Nil)) l2.head :: append (Nil, l2.tail)
		else Nil
	}
	//Exercises.append(List(1,2,3),List(3,2,1))
	//List(1,2,3,3,2,1)


	// 5, 10
	/* exam-2015.pdf - Exercise 5. Discuss briefly (maximum 5-10 lines) currification. 
	Discuss if any of the functions you have defined is currified, and compare a currified 
	version and a non-currified version of the function from(n,m). */
	/* 26 Oct 2016 – exam-2016.pdf - Exercise 5. Discuss briefly (maximum 5-10 lines) 
	currification. Discuss if any of the functions you have defined is currified. Give 
	an example of a currified function if not. */
	/* From Torra Book:
	Any function of n parameters can be seen as a function that has a single parameter,
	and given it, it returns a function with n − 1 parameters. Currification is the technique 
	for making this transformation. As an example of Currification, observe that we can define the arithmetic mean
	as a function of two arguments as follows: 
	val am: ((Double, Double) => Double) = (a,b) => (a+b)/2
	but also as a function of one argument that returns another function that given one	argument it computes 
	the mean of this argument with the previous one. That is, 
	val curryAm: (Double => (Double => Double)) = (a) => { (b) => (a+b)/2 }
	The way to call these functions will be different. We will use:
	am(2,5)
	curryAm(2)(5)
	The main and important difference between currified and non currified is that we can call the latter with only 
	some of the first arguments. For example, the following	call is valid:
	curryAm(2)
	This call returns a function that computes the mean of any number with 2. We can thus define
	val meanWith2 = curryAm(2)
	and then apply this function to any other number as e.g. meanWith2(10)*/


	// 6 - combinations
	/* 26 Oct 2016 – exam-2016.pdf - Exercise 1 */
	def combinations(n: Int): List[List[Int]] = {
		
		def numToBin(l: List[Int], n: Int): List[Int] = {
			if ((n == 0) || (n == 1)) List(n) ::: l.tail
			else n % 2 :: numToBin(l.tail, n / 2)
		}
		
		def loop(first: List[Int], n: Int, acum: Int): List[List[Int]] = {
			if (acum >= Math.pow(2, n)) Nil
			else List(numToBin(first, acum).reverse) ::: loop(List.fill(n)(0), n, acum+1)
		}
		
		loop(List.fill(n)(0), n, 0)
	}
	//Exercises.combinations(3)
	//List(List(0, 0, 0), List(0, 0, 1), List(0, 1, 0), List(0, 1, 1), List(1, 0, 0), List(1, 0, 1), List(1, 1, 0), List(1, 1, 1))


	// 7 - secondSmallest
	/* 26 Oct 2016 – exam-2016.pdf - Exercise 2 */	
	def secondSmallest(l: List[Int]): Int = {

		//Nth smallest number
		def smallest(l: List[Int], nth: Int): Int = {

		  	// insert(1,List(3)) = 3 :: insert(1,Nil) = 3 :: 1 :: Nil	
		  	def insert(e: Int, sn: List[Int]): List[Int] = sn match {
			    case Nil => e :: Nil
			    case h :: t => if (e > h) e :: sn else h :: insert(e, t)
		  	}

		  	def loop(l2: List[Int], smallest: List[Int], size: Int): Int = l2 match {
			    case Nil => smallest.head
			    case h :: t => if (size < nth) loop(t, insert(h, smallest), size + 1)
			    			else if (h < smallest.head) loop(t, insert(h, smallest.tail), size)
			    			else loop(t, smallest, size)
		  	}

	  		loop(l, Nil, 0)
		}

		/*
		smallest(List(3, 7, 1, 9), 2)
		= loop(List(3, 7, 1, 9), Nil, 0)
		= loop(List(7, 1, 9), insert(3, Nil), 1)               insert -> List(3)
		= loop(List(1, 9), insert(7, List(3)), 2)              insert -> List(7,3)
		= loop(List(9), insert(1, List(3)), 2)                 insert -> List(3,1)
		= loop(Nil, List(3, 1), 2)
		= 3
		*/
	
		smallest(l, 2)
	}
	//Exercises.secondSmallest(List(6,4,5,1,3,2))
	//2


	// 8, 14, 20, 24 - stream2WordsRec
	/* 26 Oct 2016 – exam-2016.pdf - Exercise 3 */
	/* 19 Dec 2016 – IT712A161219.pdf - Exercise 4 */
	/* 23 Oct 2017 – exam-2017.pdf - Exercise 5 */
	/* 4 Jan 2018 – linl_2018-02-06_11-20-32_3.pdf - Exercise 4 */
	def stream2WordsRec(s: Stream[Char], separator: Char => Boolean, 
			f: List[List[Char]]): Stream[List[Char]] = {
		
		def sw(s: Stream[Char], separator: Char => Boolean, 
				p: Stream[List[Char]], v: List[Char]): Stream[List[Char]] = s match {
			case Stream.Empty => p #::: Stream(v)
			case h #:: t => if (separator(h)) sw(t, separator, p #::: Stream(v), Nil)
							else sw(t, separator, p, v :+ h)
		}

		sw(s, separator, Stream.Empty, Nil)
	}
	//Exercises.stream2WordsRec(('H'#::'e'#::'l'#::'l'#::'o'#::' '#::'I'#::' '#::'a'#::'m'#::' '#::'a'#::' '#::'c'#::'a'#::'t'#::Stream.empty),_ == ' ', Nil).take(5).toList
	//List(List(H, e, l, l, o), List(I), List(a, m), List(a), List(c, a, t))


	// 9, 15 - append with higher order function
	/* 26 Oct 2016 – exam-2016.pdf - Exercise 4 */
	/* 19 Dec 2016 – IT712A161219.pdf - Exercise 5 */
	def appendHOF(l1: List[Int], l2: List[Int]): List[Int] = {
		
		def loop(l: List[(Int, Int)]): List[Int] = l match {
			case Nil => Nil
			case h :: t => (h._1 :: loop(t)) :+ h._2
		}

		loop(l1.zip(l2.reverse))
	}
	//Exercises.appendHOF(List(1,2,3),List(4,5,6))
	//List(1,2,3,4,5,6)


	// 16 - euclidean
	/* 23 Oct 2017 – exam-2017.pdf - Exercise 1 */
	def euclidean(x: List[Double], y: List[Double]): Double = {
		def euclidean_2(x: List[Double], y: List[Double], curr: Double): Double = {
			if (x.isEmpty || y.isEmpty) curr
			else euclidean_2(x.tail, y.tail, curr + Math.pow(x.head - y.head, 2))
		}
		Math.sqrt(euclidean_2(x, y, 0.0))
	}
	//Exercises.euclidean(List(1.0,2.0),List(10.0,11.0))
	//12.727922061357855

	def euclidean_v2(x: List[Double], y: List[Double]): Double = {
		Math.sqrt(x.zip(y).map(x => Math.pow(x._1 - x._2, 2)).foldLeft(0.0)((a, b) => a + b))
	}
	//Exercises.euclidean_v2(List(1.0,2.0),List(10.0,11.0))
	//12.727922061357855


	// 17 - selectedMean
	/* 23 Oct 2017 – exam-2017.pdf - Exercise 2 */
	def selectedMean(l: List[(Double,Int)], n: Int): Double = {
		def selectedMean_2(l: List[(Double,Int)], accu: Double): Double = {
			if (l.isEmpty) accu
			else selectedMean_2(l.tail, accu + l.head._1)
		}
		selectedMean_2(l.filter(_._2 == n), 0) / l.filter(_._2 == n).size
	}
	//Exercises.selectedMean(List((2.0,0),(4.5,1),(1.2,1),(3.0,3),(4.4,1),(4.5,0),(1.7,0),(5.3,2),(2.0,3)),3)
	//2.5


	// 18 - NewtonMethod
	/* 23 Oct 2017 – exam-2017.pdf - Exercise 3 */	
	def NewtonMethod (n: Double, f: Double => Double, f1: Double => Double): Stream[Double] = {
		((n) - ((f(n))/(f1(n)))) #:: NewtonMethod(((n+1) - ((f(n+1))/(f1(n+1)))), f, f1)
	}
	//Exercises.NewtonMethod(10, (x) => { x*x - 8.0 }, (x) => { 2*x }).take(5).toList
	//List(5.4, 3.6139887244538405, 3.003663183087033, 2.8627835200621603, 2.8388304511393923)

	val NewtonMethodVal: (Double, (Double => Double), (Double => Double)) => Stream[Double] = (n, f, f1) => {
		((n) - ((f(n))/(f1(n)))) #:: NewtonMethod(((n+1) - ((f(n+1))/(f1(n+1)))), f, f1)
	}
	//Exercises.NewtonMethodVal(10, (x) => { x*x - 8.0 }, (x) => { 2*x }).take(5).toList
	//List(5.4, 3.6139887244538405, 3.003663183087033, 2.8627835200621603, 2.8388304511393923)


	// 19 - addAll
	/* 23 Oct 2017 – exam-2017.pdf - Exercise 4 */	
	def addAll(l: List[List[Int]]): Int = {
		def addAll_2(l: List[Int], n: Int): Int = {
			if (l.isEmpty) n
			else addAll_2(l.tail, n + l.head)
		}
		def loop(l: List[List[Int]], sum: Int): Int = {
			if (l.isEmpty) sum
			else loop(l.tail, sum + addAll_2(l.head, 0))
		}
		loop(l,0)
	}
	//Exercises.addAll(List(List(1,2,3),List(4,5,6)))
	//21


	// 21 - manhattan
	/* 4 Jan 2018 – linl_2018-02-06_11-20-32_3.pdf - Exercise 1 */
	def manhattan(x: List[Double], y: List[Double]): Double = {
		def manhattan_2(x: List[Double], y: List[Double], curr: Double): Double = {
			if (x.isEmpty || y.isEmpty) curr
			else manhattan_2(x.tail, y.tail, curr + Math.abs(x.head - y.head))
		}
		manhattan_2(x,y,0.0)
	}
	//Exercises.manhattan(List(1.0,2.0),List(10.0,11.0))
	//18.0

	def manhattan_v2(x: List[Double], y: List[Double]): Double = {
		x.zip(y).map(x => Math.abs(x._1 - x._2)).foldLeft(0.0)((a,b) => a + b)
	}
	//Exercises.manhattan_v2(List(1.0,2.0),List(10.0,11.0))
	//18.0


	// 22 - addSelected and addAll
	/* 4 Jan 2018 – linl_2018-02-06_11-20-32_3.pdf - Exercise 2 */
	def addSelected(l: List[(Double,Int)], n: Int): Double = {
		
		def addSelected_2(l: List[(Double,Int)], accu: Double): Double = {
			if (l.isEmpty) accu
			else addSelected_2(l.tail, accu + l.head._1)
		}
		
		addSelected_2(l.filter(_._2 == n), 0)
	}
	//Exercises.addSelected(List((2.0,0),(4.5,1),(1.2,1),(3.0,3),(4.4,1),(4.5,0),(1.7,0),(5.3,2),(2.0,3)),3)
	//5

	def addAll2Num(l: List[(Double,Int)], n: Int, sum: Double): Double = l match {
		case Nil => sum
		case h :: t => if (h._2 == n) addAll2Num(t, n, sum + h._1)
					   else addAll2Num(t, n, sum)
	}
	def addAll2(l1: List[(Double,Int)], l2: List[Int]): List[Double] = l2 match {
		case Nil => Nil
		case h :: t => addAll2Num(l1, h, 0) :: addAll2(l1, t)
	}
	//Exercises.addAll2(List((2.0,0),(4.5,1),(1.2,1),(3.0,3),(4.4,1),(4.5,0),(1.7,0),(5.3,2),(2.0,3)),List(0,1,2,3))
	//List(8.2, 10.100000000000001, 5.3, 5.0)


	// 23 - seqSin
	/* 4 Jan 2018 – linl_2018-02-06_11-20-32_3.pdf - Exercise 3 */
	def seqSin(): Stream[Double] = {
		def seqSin2(i: Int = 1): Stream[Double] = math.sin(i / 2) #:: seqSin2(i + 1)
		seqSin2()
	}
	//Exercises.seqSin().take(5).toList
	//List(0.0, 0.8414709848078965, 0.8414709848078965, 0.9092974268256817, 0.9092974268256817)

}