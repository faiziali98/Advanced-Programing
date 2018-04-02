package main
import "fmt"

func Merge(left,right []int) []int {
	merged := make([]int,0, len(left)+len(right))
	for len(left)>0 || len(right)>0 {
		if len(left) == 0{
			return 
		}else if len(right)==0{

		}else if left[0]<=right[0]{
			merged = append(merged, left[0])
		}else{
			merged = append(merged, right[0])
		}
	}
}

func MergeSort(data []int) []int{
	if len(data)<=1{
		return data
	}
	mid := len(data)/2
	var left []int
	done := make(chan bool)
	go func(){
		left := MergeSort(data[:mid])
		//send message "true"
		done<-true
	}()
	right := MergeSort(data[mid:])
	//recv message
	<-done
	return Merge(left,right)
}

func main(){
	data := []int{9,44,5,2,3,1,2,3,1,7,8,9}
	fmt.Printf("%v\n%v\n",data,MergeSort(data))
}