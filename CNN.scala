package pplAssignment

object F2016A7PS0140P {
  
  def dotProduct(matrix_1:List[List[Double]],matrix_2: List[List[Double]]):Double={
    //compute prod of 1d list
    def prod1d(m1:List[Double],m2:List[Double],ans:Double):Double= (m1,m2) match{
      case (Nil,Nil)=>ans
      case (x1::xs,y1::ys)=>prod1d(xs,ys,ans+x1*y1)
      case(_,_)=>0.0//to remove warnings
    }
    //to compute dot product of 2d list
    def prod2d(m1:List[List[Double]],m2:List[List[Double]],ans:Double):Double=(m1,m2) match{
      case (Nil,Nil)=> ans
      case (x1::xs,y1::ys)=>prod2d(xs,ys,prod1d(x1,y1,0.0)+ans)
      case(_,_)=>0.0//to remove warnings
    }
  	//println(matrix_1)
    prod2d(matrix_1,matrix_2,0.0)
  }
  
  
  def convolute(Image:List[List[Double]],Kernel:List[List[Double]],imageSize:List[Int],kernelSize:List[Int]):List[List[Double]]={
    
    //compute single row DotProduct 
  	def compute(im:List[List[List[Double]]],Kernel:List[List[Double]],ans:List[Double]):List[Double]=im match{
      case Nil=>ans
      case x::xs=>compute(xs,Kernel,ans:::(List(dotProduct(x,Kernel))))
    }
    //apply it on a 2d list
    def apply2d(im:List[List[List[List[Double]]]],Kernel:List[List[Double]],ans:List[List[Double]]):List[List[Double]]=im match{
      case Nil=>ans
      case x::xs=>apply2d(xs,Kernel,ans:::(List(compute(x,Kernel,List[Double]()))))
    }
    
    /**for rows **/
    //extract K1 rows in im
    def temp(im:List[List[Double]],K1:Int,ans:List[List[Double]]):List[List[Double]]=im match{
      case Nil=>ans
      case x::xs=> if(K1==0)ans else temp(xs,K1-1,ans:::(List(x)))
    }
    
    //extract set of consecutive K1 rows
    def temp1(im:List[List[Double]],I1:Int,K1:Int,ans:List[List[List[Double]]]):List[List[List[Double]]]=im match{
      case Nil=>ans
      case x::xs=>if(I1>=K1){
        val l1=temp(im,K1,List[List[Double]]())
        temp1(xs,I1-1,K1,ans:::List(l1))
      }
      else ans
    }
    
    /**for columns **/
    //extract a size of K2 elements
    def temp3(im:List[Double],K2:Int,ans:List[Double]):List[Double]=im match{
      case Nil=>ans
      case x::xs=> if(K2==0)ans else temp3(xs,K2-1,ans:::(List(x)))
    }
    
    //extract set of consecutive K2 columns for a row
    def temp4(im:List[Double],I2:Int,K2:Int,ans:List[List[Double]]):List[List[Double]]=im match{
      case Nil=>ans
      case x::xs=>if(I2>=K2){
        val l1=temp3(im,K2,List[Double]())
        temp4(xs,I2-1,K2,ans:::List(l1))
      }
      else ans
    }
    
    //make a list of list of list of K2 elements for a 2d list 
    def temp6(im:List[List[Double]],I2:Int,K2:Int,ans:List[List[List[Double]]]):List[List[List[Double]]]=im match{
      case Nil=>ans
      case x::xs=>temp6(xs,I2,K2,ans:::(List(temp4(x,I2,K2,List[List[Double]]()))))
    }
    /***End Columns All Working**/
  
  	
  	//function to append single row in a form of matrix
    def app(ls:List[List[Double]],l1:List[List[List[Double]]],l2:List[List[List[Double]]]):List[List[List[Double]]]=(ls,l1) match{
      case (Nil,Nil)=>l2
      case (x::xs,Nil)=> app(xs,Nil,l2:::(List(List(x))))
      case (x::xs,y::ys)=>app(xs,ys,(List(y:::(List(x)))).:::(l2))
      case (Nil,List(_))=>l2//to remove warnings
    }
  	//calling app for all rows in possible combination i.e. one list of list of list
    def temp7(im:List[List[List[Double]]],ans:List[List[List[Double]]]):List[List[List[Double]]]=im match{
      case Nil=>ans
      case x::xs=> temp7(xs,app(x,ans,List[List[List[Double]]]()))
    }
    
  	//temp8 works on lists for all row combination possible and generates possible output
    def temp8(ls:List[List[List[List[Double]]]],ans:List[List[List[List[Double]]]]):List[List[List[List[Double]]]]=ls match{
      case Nil=>ans
      case x::xs=>temp8(xs,ans:::(List(temp7(x,List[List[List[Double]]]()))))
    }
    
    //function to make list of all row combinations using temp6 for further use
    def temp9(ls:List[List[List[Double]]],ans:List[List[List[List[Double]]]],K2:Int,I2:Int):List[List[List[List[Double]]]]=ls match{
      case Nil=>ans
      case x::xs=>temp9(xs,ans:::(List(temp6(x,I2,K2,List[List[List[Double]]]()))),K2,I2)
    }
  	//temp5 works on the final result
    def temp5(im:List[List[Double]],imageSize:List[Int],kernelSize:List[Int],Kernel:List[List[Double]]):List[List[Double]]={
      val l1=temp1(im,imageSize.head,kernelSize.head,List[List[List[Double]]]())
      val l2=temp9(l1,List[List[List[List[Double]]]](),kernelSize.tail.head,imageSize.tail.head)
      //println(l2)
      val l3=temp8(l2,List[List[List[List[Double]]]]())
      apply2d(l3, Kernel,List[List[Double]]())
      }
    temp5(Image,imageSize,kernelSize,Kernel)
  
}
  
  def activationLayer(activationFunc:Double => Double,Image:List[List[Double]]):List[List[Double]]={
    
    //apply activation function on 1d list
    def apply1d(f:Double=>Double,Image:List[Double],ans:List[Double]):List[Double]= Image match{
      case Nil=>ans
      case x::xs=>apply1d(f,xs,ans:::List(f(x)))
    }
    ///applying activation function on the complete 2d list
    def apply2d(f:Double=>Double,Image:List[List[Double]],ans:List[List[Double]]):List[List[Double]]=Image match{
      case Nil=>ans
      case x::xs=>apply2d(f,xs,ans:::(List(apply1d(f,x,List[Double]()))))
    }
    apply2d(activationFunc,Image,List[List[Double]]())
  }
  
  
  def singlePooling(poolingFunc:List[Double]=>Double,Image:List[List[Double]],K:Int):List[Double]={
    
    // to apply poolingFunc to the list and generate final result
    def temp(func:List[Double]=>Double,im:List[List[Double]],ls:List[Double]):List[Double]=im match{
      case Nil=>ls
      case x::xs=> temp(func,xs,ls:::List(func(x)))
    }
    //to extract a list of list of K elements in a single row
    def extractK(im:List[Double],K:Int,
      count:Int,ans:List[List[Double]],
      acc:List[Double]):List[List[Double]]=im match{
      case Nil=>List(acc).:::(ans)
      case x::xs => if(count == K) extractK(im,K,0,List(acc).:::(ans),List[Double]())
      				else extractK(xs,K,count+1,ans,List(x).:::(acc))
      }

	//to divide all rows of 2d list into size of K sized sub-lists
	def extractK2d(im:List[List[Double]],K:Int,ans:List[List[List[Double]]]):List[List[List[Double]]]=im match{
	  	case Nil=>ans
	  	case x::xs =>{
		  val l=extractK(x,K,0,List[List[Double]](),List[Double]())
		  extractK2d(xs,K,ans:::List(l))
		}
	}
	//append im to end of list of each list helper function for flatten
	def app(im:List[List[Double]],ans:List[List[Double]],l3:List[List[Double]]):List[List[Double]]=(im,ans) match{
	  case (Nil,Nil)=>l3
	  case (x::xs,y::ys)=>app(xs,ys,(List(y:::x)).:::(l3))
	  case (Nil,List(_)) =>l3 //to remove warnings
	  case (List(_),Nil) =>l3 //to remove warnings 
	  
	}
	//Flatten a list of list of list by merging each columns into list of list
	def flatten(im:List[List[List[Double]]],ans:List[List[Double]]):List[List[Double]]=im match{
  		case Nil=>ans
  		case x::xs=>flatten(xs,app(x,ans,List[List[Double]]()))
	}
	
	val t1=extractK2d(Image,K,List[List[List[Double]]]())
	val t2=flatten(t1.tail,t1.head)
  temp(poolingFunc,t2,List[Double]())
  }
  
  
  
  def poolingLayer(poolingFunc:List[Double]=>Double,Image:List[List[Double]],
      K:Int):List[List[Double]]={
      
    //function to generate list of k consecutive rows in each list i.e each element is a of dim K*M
      def extractKrows(im:List[List[Double]],K:Int,
      count:Int,ans:List[List[List[Double]]],
      acc:List[List[Double]]):List[List[List[Double]]]=im match{
      case Nil=>ans:::(List(acc))
      case x::xs => if(count == K) extractKrows(im,K,0,ans:::(List(acc)),List[List[Double]]())
      				else extractKrows(xs,K,count+1,ans,acc:::(List(x)))
      }
      val l1=extractKrows(Image,K,0,List[List[List[Double]]](),List[List[Double]]())
     
     //function to apply singlePooling on each generated set of size K*M
      def applySinglePooling(im:List[List[List[Double]]],K:Int,func:List[Double]=>Double,
      ans:List[List[Double]]):List[List[Double]]=im match{
      
      case Nil=>ans
      case x::xs=>applySinglePooling(xs,K,func,ans:::(List(singlePooling(func,x,K))))
      }
      applySinglePooling(l1,K,poolingFunc,List[List[Double]]())
      
  }
  
  def mixedLayer(Image:List[List[Double]],Kernel:List[List[Double]],
      imageSize:List[Int],kernelSize:List[Int],
      activationFunc:Double => Double,
      poolingFunc:List[Double]=>Double,
      K:Int):List[List[Double]]={
    
    val temp1=convolute(Image, Kernel, imageSize, kernelSize)
    val temp2=activationLayer(activationFunc, temp1)
    poolingLayer(poolingFunc, temp2, K)
  }
  
  def max(a:Double,b:Double):Double=if(a>b)a else b
  def min(a:Double,b:Double):Double=if(b>a)a else b
  
  //function to find minimum or maximum in a list
  def minmax1d(x:List[Double],m:Double,f:(Double,Double)=>Double):Double=x match{
      case Nil=>m
      case x1::xs=>minmax1d(xs,f(x1,m),f)
    }
  
  
  def normalise(Image:List[List[Double]]):List[List[Int]]={
    //finding minimum maximum in 2d
    def minmax2d(ls:List[List[Double]],m:Double,
        f:(Double,Double)=>Double):Double= ls match{
      case Nil=>m
      case x::xs=>minmax2d(xs,minmax1d(x,m,f),f)
    }
    //normalise a row
    def row(ls:List[Double],max:Double,
        min:Double,ans:List[Int]):List[Int]=ls match {
      case Nil=>ans
      case x::xs=>row(xs,max,min,
          ans:::List(scala.math.round((255.0*((x-min)/(max-min))).toFloat)))
    }
    //normalise a 2d matrix
    def mat(Image:List[List[Double]],max:Double,
        min:Double,ans:List[List[Int]]):List[List[Int]]= Image match{
      case Nil=>ans
      case x::xs =>mat(xs,max,min,ans:::List[List[Int]](row(x,max,min,List[Int]())))
    }
    
    mat(Image,minmax2d(Image,(Image.head).head,max),
        minmax2d(Image,(Image.head).head,min),List[List[Int]]())
  }
  //println(normalise(test))
  
  
  def assembly(Image:List[List[Double]],imageSize:List[Int],w1:Double,w2:Double,b:Double,
      Kernel1:List[List[Double]],kernelSize1:List[Int],
      Kernel2:List[List[Double]],kernelSize2:List[Int],
      Kernel3:List[List[Double]],kernelSize3:List[Int],Size: Int):List[List[Int]]={
      
      def relu(d:Double)=if(d>0) d else 0
      
      def leakyRelu(d:Double)=if(d>0)d else 0.5*d
      
      def poolMax(ls:List[Double]):Double={
        minmax1d(ls, ls.head, max)
      }
      
      def poolAv(ls:List[Double]):Double={
        def temp(ls:List[Double],tempSum:Double,count:Int):Double=ls match{
          case Nil=>tempSum/count
          case x::xs=>temp(xs,tempSum+x,count+1)
        }
        temp(ls,0.0,0)
      }
      //add two 2d lists of same dimensions
      def addList2d(ls1:List[List[Double]],ls2:List[List[Double]]):List[List[Double]]={
        def add1d(x:List[Double],y:List[Double],ans:List[Double]):List[Double]= (x,y) match{
      		
      		case (Nil,Nil)=>ans
      		case (x1::xs,y1::ys) =>add1d(xs,ys,ans:::List(x1+y1))
      		case (List(_), Nil)| (Nil, List(_))=>ans

        }
        //helper function
        def add2d(x:List[List[Double]],y:List[List[Double]],ans:List[List[Double]]):List[List[Double]]= (x,y) match{
      		case (Nil,Nil)=>ans
      		case (x1::xs,y1::ys) =>add2d(xs,ys,ans:::List[List[Double]](add1d(x1,y1,List[Double]())))
      		case (List(_), Nil)| (Nil, List(_))=>ans //to remove warnings

        }
        add2d(ls1,ls2,List[List[Double]]())
      }

      val mixedLayer_1=mixedLayer(Image, Kernel1, imageSize, kernelSize1, relu, poolAv, Size)
      val mixedLayer_2=mixedLayer(Image, Kernel2, imageSize, kernelSize2, relu, poolAv, Size)
      val temp1=activationLayer((x:Double)=>x*w1,mixedLayer_1)
      val temp2=activationLayer((x:Double)=>x*w2,mixedLayer_2)
      val temp3=activationLayer((x:Double)=>x+b,addList2d(temp1,temp2))
      
      val a=(imageSize.head-kernelSize1.head+1)/Size
      val c=(imageSize.tail.head-kernelSize1.tail.head+1)/Size
      
      val mixedLayer_3=mixedLayer(temp3, Kernel3, List(a,c), kernelSize3, leakyRelu, poolMax, Size)
      normalise(mixedLayer_3)
  }
}
