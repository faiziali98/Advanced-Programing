package main

import (
    "fmt"
    "os"
    "strconv"
    "math"
    "sync"
	"encoding/csv"
)


type Rectangle struct {
    Left float64
    Right float64 
    Top float64 
    Bottom float64
    Pop int
}

func (r *Rectangle) Contains(x,y float64 ) bool {
  return x>=r.Left && x<=r.Right && y>=r.Bottom && y<=r.Top
}

func (r *Rectangle) Encompass(rect Rectangle) Rectangle{
  return Rectangle{ math.Min(r.Left,rect.Left),
                    math.Max(r.Right,rect.Right),
                    math.Max(r.Top,rect.Top),
                    math.Min(r.Bottom,rect.Bottom),
                    (r.Pop+rect.Pop) }
}

type Cord struct {
    W int
    S int 
    E int 
    N int
}

type CensusGroup struct {
	population int
	latitude, longitude float64
}

type Data struct{
    grid Rectangle
    data []CensusGroup
    x int
    y int
}

func CornersSeq (data []CensusGroup, l int,h int) Rectangle{
    r := Rectangle{data[0].longitude,data[0].longitude,data[0].latitude,data[0].latitude,0}
    for i := l; i < h; i++ {
        if data[i].latitude>r.Top{
            r.Top = data[i].latitude
        }
        if data[i].latitude<r.Bottom{
            r.Bottom = data[i].latitude
        }
        if data[i].longitude>r.Right{
            r.Right = data[i].longitude
        }
        if data[i].longitude<r.Left{
            r.Left = data[i].longitude
        }

        r.Pop = r.Pop + data[i].population 
    }
    return r
}

func CalculatePopSeq(d []CensusGroup, l int, h int, r Rectangle) int{
    pop := 0

    for i := l; i < h; i++ {
        if r.Contains(d[i].longitude,d[i].latitude) {
            pop += d[i].population
        }
    }

    return pop
}

var sc = 10000

func CornersPar(data []CensusGroup, l int,h int) Rectangle{
    if h-l < sc{
        return CornersSeq(data,l,h)
    } else {
        done := make(chan Rectangle)
        go func(){
            left := CornersPar(data, l, (h+l)/2)
            done<-left
        }()
        right := CornersPar(data,(h+l)/2,h)
        left := <-done
        return left.Encompass(right)
    }
}

func CalcPopPar(d []CensusGroup, l int, h int, r Rectangle) int{
    if h-l < sc{
        return CalculatePopSeq(d,l,h,r)
    } else {
        done := make(chan int)
        go func(){
            left := CalcPopPar(d, l, (h+l)/2, r)
            done<-left
        }()
        right := CalcPopPar(d,(h+l)/2,h, r)
        left := <-done
        return left + right
    }
}

func ParseCensusData(fname string) ([]CensusGroup, error) {
	file, err := os.Open(fname)
    if err != nil {
		return nil, err
    }
    defer file.Close()

	records, err := csv.NewReader(file).ReadAll()
	if err != nil {
		return nil, err
	}
	censusData := make([]CensusGroup, 0, len(records))

    for _, rec := range records {
        if len(rec) == 7 {
            population, err1 := strconv.Atoi(rec[4])
            latitude, err2 := strconv.ParseFloat(rec[5], 64)
            longitude, err3 := strconv.ParseFloat(rec[6], 64)
            if err1 == nil && err2 == nil && err3 == nil {
                latpi := latitude * math.Pi / 180
                latitude = math.Log(math.Tan(latpi) + 1 / math.Cos(latpi))
                censusData = append(censusData, CensusGroup{population, latitude, longitude})
            }
        }
    }

	return censusData, nil
}


func GetX(grid Rectangle, c int, long float64) int {
    dx := (grid.Right - grid.Left)/float64(c)
    var x int

    if x = int(math.Floor((long-grid.Left)/dx));long == grid.Right {
        x = c-1
    }

    return x 
}

func GetY(grid Rectangle, r int, lat float64) int {
    dy := (grid.Top - grid.Bottom)/float64(r)
    var y int

    if y = int(math.Floor((lat-grid.Bottom)/dy)); lat == grid.Top {
        y = r-1
    }

    return y
}

func totPop(d Data, l int, h int) [][]int{
    pop := make([][]int, d.y)
    for i := range pop {
        pop[i] = make([]int, d.x)
    }
    data := d.data
    for i := l; i < h; i++ {
        x := GetX(d.grid,d.x, data[i].longitude)
        y := GetY(d.grid,d.y, data[i].latitude)
        pop[y][x] = pop[y][x] + data[i].population
    }
    return pop
}

func smartPop(pop [][]int, x int, y int) [][]int{
    for i := 0; i<y; i++{
        for j := 0 ; j<x ; j++ {
            var up int 
            if up = 0; i>0 {
                up = pop[i-1][j]
            }
            var left int
            if left = 0; j>0 {
                left = pop[i][j-1]
            }
            var diag int 
            if diag = 0; i>0 && j>0{
                diag = pop[i-1][j-1]
            }
            pop[i][j] = pop[i][j] + up + left - diag
        }
    }
    return pop
}

var scp = 3

func smartPopParCol(pop [][]int,lx int, hx int){
    if hx-lx < scp{
        for i:= 1; i<len(pop);i++{
            for j:=lx; j<hx;j++{
                pop[i][j] = pop[i][j]+pop[i-1][j]
            }   
        }
    }else{
        done := make(chan bool)
        go func(){
            smartPopParCol(pop, lx, (hx+lx)/2)
            done<-true
        }()
        smartPopParCol(pop,(hx+lx)/2,hx)
        <-done
    }
}


func smartPopParRow(pop [][]int,ly int, hy int){
    if hy-ly < scp{
        for i:= ly; i<hy;i++{
            for j:=1; j<len(pop[i]);j++{
                pop[i][j] = pop[i][j]+pop[i][j-1]
            }   
        }
    }else{
        done := make(chan bool)
        go func(){
            smartPopParRow(pop, ly, (hy+ly)/2)
            done<-true
        }()
        smartPopParRow(pop,(hy+ly)/2,hy)
        <-done
    }
}

func CalcPopSmart(pop [][] int, v Cord, x int) int{
    var topRight int
    if topRight = 0; v.S-2 >= 0 {
        topRight = pop[v.S-2][v.E-1]
    }
    var bottomLeft int
    if bottomLeft = 0; v.W-2 >= 0 {
        bottomLeft = pop[v.N-1][v.W-2]
    }
    var leftRight int
    if leftRight = 0; v.S-2 >= 0 && v.W >= 0 {
        leftRight = pop[v.S-2][v.W-2]
    }
    return pop[v.N-1][v.E-1] - topRight - bottomLeft + leftRight
}

func JoinArray(l [][]int, r [][]int) [][] int{
    for i := 0; i<len(l); i++{
        for j := 0 ; j<len(l[i]) ; j++ {
            l[i][j] = l[i][j] + r[i][j]
        }
    }
    return l
}

func SmartAndParPop(d Data, l int, h int) [][]int {
    if h-l < sc{
        return totPop(d,l,h)
    }else{
        done := make(chan [][]int)
        go func(){
            left := SmartAndParPop(d, l, (h+l)/2)
            done<-left
        }()
        right := SmartAndParPop(d,(h+l)/2,h)
        left := <-done
        return JoinArray(left, right)
    }
}

var locks [][] sync.Mutex
var popglobal [][] int

func update(i int, j int, pop int) {
    // fmt.Println(popglobal)
    locks[i][j].Lock()
    popglobal[i][j] = popglobal[i][j] + pop
    locks[i][j].Unlock()
}

func smartAndLock(d Data,l int, h int) {
    data := d.data

    for m:=l;m<h;m++{
        i := GetX(d.grid,d.x,data[m].longitude)
        j := GetY(d.grid,d.y,data[m].latitude)
        // fmt.Println(m)
        update(j,i,data[m].population)
    }
}


func main () {
	if len(os.Args) < 4 {
		fmt.Printf("Usage:\nArg 1: file name for input data\nArg 2: number of x-dim buckets\nArg 3: number of y-dim buckets\nArg 4: -v1, -v2, -v3, -v4, -v5, or -v6\n")
		return
	}
	fname, ver := os.Args[1], os.Args[4]
    xdim, err := strconv.Atoi(os.Args[2])
	if err != nil {
		fmt.Println(err)
		return
	}
    ydim, err := strconv.Atoi(os.Args[3])
	if err != nil {
		fmt.Println(err)
		return
	}
	censusData, err := ParseCensusData(fname)
	if err != nil {
		fmt.Println(err)
		return
	}
    fmt.Printf("%v\n", len(censusData))
    // Some parts may need no setup code
    var grid Rectangle
    var pop [][]int

    switch ver {
    case "-v1":
        grid = CornersSeq(censusData, 0, len(censusData))
    case "-v2":
        grid = CornersPar(censusData, 0, len(censusData))
    case "-v3":
        grid = CornersSeq(censusData, 0, len(censusData))
        d := Data{grid,censusData,xdim,ydim}
        pop = totPop(d,0,len(censusData))
        pop = smartPop(pop, xdim,ydim)
    case "-v4":
        // YOUR SETUP CODE FOR PART 4
        grid = CornersPar(censusData, 0, len(censusData))
        d := Data{grid,censusData,xdim,ydim}
        pop = SmartAndParPop(d,0,len(censusData))
        pop = smartPop(pop, xdim,ydim)
    case "-v5":
        // YOUR SETUP CODE FOR PART 5
        grid = CornersPar(censusData, 0, len(censusData))
        d := Data{grid,censusData,xdim,ydim}
        size := len(censusData)

        popglobal = make([][]int, ydim)
        for i := range popglobal {
            popglobal[i] = make([]int, xdim)
        }
        locks = make([][]sync.Mutex, ydim)
        for i := range locks {
            locks[i] = make([]sync.Mutex, xdim)
        }

        done := make(chan bool)
        done1 := make(chan bool)
        done2 := make(chan bool)
        go func(){
            smartAndLock(d, 0, size/4)
            done<-true
        }()
        go func(){
            smartAndLock(d,size/4,size/2)
            done1<-true
        }()
        go func(){
            smartAndLock(d, size/2, (size*3)/4)
            done2<-true
        }()
        smartAndLock(d,(size*3)/4, size)
        <-done
        <-done1
        <-done2

        pop = smartPop(popglobal, xdim,ydim)
    case "-v6":
        // YOUR SETUP CODE FOR PART 6
        grid = CornersPar(censusData, 0, len(censusData))
        d := Data{grid,censusData,xdim,ydim}
        pop = SmartAndParPop(d,0,len(censusData))
        smartPopParRow(pop,0,ydim)
        smartPopParCol(pop,0,xdim)

    default:
        fmt.Println("Invalid version argument")
        return
    }

    lx := (grid.Right - grid.Left)/float64(xdim);
    ly := (grid.Top - grid.Bottom)/float64(ydim);
    xadd := grid.Left
    yadd := grid.Bottom
    _ = pop
    fmt.Println(grid.Left," ", grid.Right, " ", grid.Bottom, " ", grid.Top)

    for {
        var west, south, east, north int
        n, err := fmt.Scanln(&west, &south, &east, &north)

        if n != 4 || err != nil || west<1 || west>xdim || south<1 || south>ydim || east<west || east>xdim || north<south || north>ydim {
            break
        }

        var population int
        var percentage float64
        switch ver {
        case "-v1":
            // YOUR QUERY CODE FOR PART 1
            r := Rectangle{(float64(west-1)*lx)+xadd, (float64(east)*lx)+xadd,
                (float64(north)*ly)+yadd,(float64(south-1)*ly)+yadd,0}

            population = CalculatePopSeq(censusData, 0, len(censusData), r)
            percentage = (float64(population)/float64(grid.Pop))*100.0
        case "-v2":
            // YOUR QUERY CODE FOR PART 2
            r := Rectangle{(float64(west-1)*lx)+xadd, (float64(east)*lx)+xadd,
                (float64(north)*ly)+yadd,(float64(south-1)*ly)+yadd,0}

            population = CalcPopPar(censusData, 0, len(censusData), r)
            percentage = (float64(population)/float64(grid.Pop))*100.0
        case "-v3":
            // YOUR QUERY CODE FOR PART 3
            cor := Cord{west, south, east, north}
            population = CalcPopSmart(pop,cor,xdim)
            percentage = (float64(population)/float64(grid.Pop))*100.0
        case "-v4":
            // YOUR QUERY CODE FOR PART 4
            cor := Cord{west, south, east, north}
            population = CalcPopSmart(pop,cor,xdim)
            percentage = (float64(population)/float64(grid.Pop))*100.0
        case "-v5":
            // YOUR QUERY CODE FOR PART 5
            cor := Cord{west, south, east, north}
            population = CalcPopSmart(pop,cor,xdim)
            percentage = (float64(population)/float64(grid.Pop))*100.0
        case "-v6":
            // YOUR QUERY CODE FOR PART 6
            cor := Cord{west, south, east, north}
            population = CalcPopSmart(pop,cor,xdim)
            percentage = (float64(population)/float64(grid.Pop))*100.0
        }

        fmt.Printf("%v %.2f%%\n", population, percentage)
    }
}
