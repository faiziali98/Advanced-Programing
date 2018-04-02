package main
import "fmt"

var sc = 3

func longest_sequence(n,lo,hi int, arr []int) []int{
	lsqe := 0
	lend := 0
	seqlen := 0
	jkee := 0
	lbeg := 0

	for i:=lo;i<hi;i++ {
		if arr[i] == n{
			for j:=i; j<hi; j++{
				if (arr[j]==n){
					seqlen += 1
					jkee = j
				}else{
					break
				}
			}
		}
		if (seqlen>=lsqe){
			lsqe = seqlen
			lend = jkee
			lbeg = i
		}
		seqlen = 0
		i = jkee
	}
	toret := []int{arr[lo],arr[hi-1],lsqe,lbeg,lend}
	return toret
}

func lseq_parr(n,lo,hi int, arr []int) []int{
	if hi-lo <= sc{
		return longest_sequence(n,lo,hi,arr)
	}else{
		done := make(chan []int)
		go func(){
			left := lseq_parr(n,lo,(hi+lo)/2,arr)
			done<-left
		}()
		right := lseq_parr(n,(hi+lo)/2,hi,arr)
		left := <-done
		lseq := 0
		lbeg := 0
		lend := 0

		if left[2]==0 {
			lseq = right[2]
		}else if right[2]==0 {
			lseq = left[2]
		}else{
			if (left[1]==right[0] && left[1]==n && left[4] == (right[3]-1)){
				fmt.Println("came",left,right)
				lseq = left[2]+right[2]
				lbeg = left[3]
				lend = right[4]
			}else{
				if left[2]<right[2]{
					lseq = right[2]
					lbeg = right[3]
					lend = right[4]
				}else{
					lseq = left[2]
					lbeg = left[3]
					lend = left[4]
				}
			}
		}
		toret := []int{left[0],right[1],lseq,lbeg,lend}
		return toret
	}
}

func main() {
	arr := []int{3,3,4,4,4,3,3,4,3,3,3,3}
	fmt.Println(lseq_parr(3,0,len(arr),arr))
}
