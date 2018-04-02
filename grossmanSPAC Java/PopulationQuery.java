import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Scanner;
import java.util.Arrays;
import java.math.BigDecimal;
import java.util.concurrent.ForkJoinTask;
import java.util.concurrent.Callable;

public class PopulationQuery {
	// next four constants are relevant to parsing
	public static final int TOKENS_PER_LINE  = 7;
	public static final int POPULATION_INDEX = 4; // zero-based indices
	public static final int LATITUDE_INDEX   = 5;
	public static final int LONGITUDE_INDEX  = 6;
  static final int SEQUENTIAL_CUTOFF = 1000;

	// parse the input file into a large array held in a CensusData object
	public static CensusData parse(String filename) {
		CensusData result = new CensusData();
      try {
        BufferedReader fileIn = new BufferedReader(new FileReader(filename));

        String oneLine = fileIn.readLine();
        while ((oneLine = fileIn.readLine()) != null) {
            String[] tokens = oneLine.split(",");
            if(tokens.length != TOKENS_PER_LINE)
            	throw new NumberFormatException();
            int population = Integer.parseInt(tokens[POPULATION_INDEX]);
            if(population != 0)
            	result.add(population,
            			   Float.parseFloat(tokens[LATITUDE_INDEX]),
          		       Float.parseFloat(tokens[LONGITUDE_INDEX]));
        }
          fileIn.close();
        } catch(IOException ioe) {
          System.err.println("Error opening/reading/writing input or output file.");
          System.exit(1);
        } catch(NumberFormatException nfe) {
          System.err.println(nfe.toString());
          System.err.println("Error in file format");
          System.exit(1);
    		}
      return result;
	}

	// ForkJoinTask Calculations
	public static float [] joinArr(float [] l, float [] r){
		float [] toRet = new float[5];
		toRet[0] = l[0]>r[0] ? l[0]:r[0];
		toRet[1] = l[1]<r[1] ? l[1]:r[1];
		toRet[2] = l[2]>r[2] ? l[2]:r[2];
		toRet[3] = l[3]<r[3] ? l[3]:r[3];
		toRet[4] = l[4] + r[4];
		return toRet;
	}

	static int compPop(CensusGroup[] arr, int lo, int hi, Rectangle r) {
	  if (hi-lo < SEQUENTIAL_CUTOFF) {
		  return calculatePop(arr, lo, hi, r);
	  } else {
		  ForkJoinTask<Integer> left =
		  					ForkJoinTask.adapt(()->compPop(arr, lo, (hi+lo)/2, r)).fork();
		  int rightAns = compPop(arr, (hi+lo)/2, hi, r);
		  int leftAns = left.join();
		  return leftAns + rightAns;
	  }
  }

	static float [] compCor(CensusGroup[] arr, int lo, int hi) {
	  if (hi-lo < SEQUENTIAL_CUTOFF) {
		  return corners(arr, lo, hi);
	  } else {
		  ForkJoinTask<float[]> left =
		  					ForkJoinTask.adapt(()->compCor(arr, lo, (hi+lo)/2)).fork();
		  float[] rightAns = compCor(arr, (hi+lo)/2, hi);
		  float[] leftAns = left.join();
			return joinArr(leftAns, rightAns);
	  }
  }

	// Sequential Calculations
	public static int calculatePop(CensusGroup[] d, int l, int h, Rectangle r){
		int pop = 0;
		for(int i = l; i<h; i++){
			if (r.contains(d[i].longitude, d[i].latitude, i)){
				pop += d[i].population;
			}
		}
		return pop;
	}

	public static float [] corners(CensusGroup[] data, int l, int h){
		float []toRet =  {data[0].longitude, data[0].longitude,
											data[0].latitude, data[0].latitude, 0};
		for(int i = l; i<h; i++){
			if (data[i].longitude>toRet[0]){
				toRet[0] = data[i].longitude;
			}else if (data[i].longitude<toRet[1]){
				toRet[1] = data[i].longitude;
			}else if (data[i].latitude>toRet[2]){
				toRet[2] = data[i].latitude;
			}else if (data[i].latitude<toRet[3]){
				toRet[3] = data[i].latitude;
			}
			toRet[4] += data[i].population;
		}
		return toRet;
	}

	// Helper Functions
	public static void results(int pop, float totalPop){
		System.out.println("Population in this area is: " + pop);
		System.out.println("In percentage: " + precs(2,(pop/totalPop)*100)+"%\n");
	}

	public static int [] inputs(){
		System.out.println("Enter the four arguments in WSEN order");
		Scanner in = new Scanner(System.in);
		int [] toRet = {in.nextInt(), in.nextInt(),
										in.nextInt(), in.nextInt()};
		return toRet;
	}

	public static float precs(int decimalPlace, float d) {
	  BigDecimal bd = new BigDecimal(Float.toString(d));
	  bd = bd.setScale(decimalPlace, BigDecimal.ROUND_HALF_UP);
	  return bd.floatValue();
	}

	// Main program
	public static void main(String[] args) {
		CensusData cd = parse(args[0]);
		int x = Integer.parseInt(args[1]);
		int y = Integer.parseInt(args[2]);
		float [] rec = compCor(cd.data, 0, cd.data_size);
		Rectangle grid = new Rectangle(rec[1], rec[0], rec[2], rec[3]);

		System.out.println(grid.toString());
		float lx = (grid.right - grid.left)/x;
		float ly = (grid.top - grid.bottom)/y;

		try {
			while(true){
				int [] v = inputs();
				if (v[0]>=1 && v[0]<=x && v[1]>=1 && v[1]<=y &&
											v[2]>=v[0] && v[2]<=x && v[3]>=v[1] && v[3]<=y){
					float xmg = grid.left;
					float ymg = grid.bottom;
					Rectangle uRect = new Rectangle((v[0]-1)*lx+xmg, v[2]*lx+xmg,
																					v[3]*ly+ymg, (v[1]-1)*ly+ymg );
					int pop = compPop(cd.data, 0, cd.data_size, uRect);
					results(pop,rec[4]);
				}else{
					throw new NewException("Invalid Input");
				}
			}
		}catch(NewException e){
			System.out.println(e.error);
		}
	}
}
