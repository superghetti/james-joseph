import java.util.Scanner;

public class BinaryCalculator
{
	public static void main(String[] args)
	{
		System.out.println("Welcome to the BinaryCalculator");
		Scanner in = new Scanner(System.in);
		while(true){
			String arg1;
			if(in.hasNext()){
				arg1 = in.next();
				if(arg1.equalsIgnoreCase("QUIT")){
					break;
				}
			}
			else {
				break;
			}
			String operator = "?", arg2 = "0";
			if(in.hasNext()){
				operator = in.next();
			}
			if(in.hasNext()){
				arg2 = in.next();
			}
			if(arg1.length() != arg2.length()){
				System.err.println("ERROR: '" + arg1 + "' and '" + arg2 + "' are not the same length.");
				continue;
			}

			// TODO: the strings arg1 and arg2 are all 1 or 0, convert to an array of booleans
			try {
				boolean[] bits1 = string2bits(arg1);
				boolean[] bits2 = string2bits(arg2);
				boolean[] result = new boolean[0];
				boolean[] remainder = {false};
				Object[] ob = new Object[0];
				//String remainderBit = "";
				String remainderStr = "";
				String error = "";


				// TODO: using the "operator" variable, do the appropriate operations
				if(operator.equalsIgnoreCase("+")) {
					result = add(bits1, bits2);
				}else if(operator.equalsIgnoreCase("-")) {
					result = subtract(bits1, bits2);
				}else if(operator.equalsIgnoreCase("*")) {
					result = multiply(bits1, bits2);
				}else if(operator.equalsIgnoreCase("/")) {
					ob = divide(bits1, bits2);
					result = (boolean[]) ob[0];
					remainder = (boolean[]) ob[1];
					if(ob.length == 3) {
						error = (String) ob[2];
						remainderStr = "";
					}else {
						remainderStr = "R" + bits2decimal(remainder);
					}
					if(ob.length == 3) {
						error = (String) ob[2];
					}
				}else{
					throw new Exception("Invalid sign");
				}

				// TODO: print out the result in both binary and decimal.

				// Binary (for debugging):
				// TODO: always use "System.err" for debugging in this project
				//							System.err.println(String.format("DEBUG: %s %s %s = %s%s",
				//									bits2string(bits1), operator, bits2string(bits2), bits2string(result), remainderBit));
				// Decimal (actual output):
				if(ob.length == 3) {
					System.out.println(String.format("%d %s %d = %s",
							bits2decimal(bits1), operator, bits2decimal(bits2), error));
				}else {
					System.out.println(String.format("%d %s %d = %d%s%s",
							bits2decimal(bits1), operator, bits2decimal(bits2), bits2decimal(result), remainderStr, error));
				}
			}catch(Exception e) {
				//e.printStackTrace();
				System.err.println(e.getMessage());
			}
		}
		in.close();
	}

	private static boolean[] string2bits(String arg1) throws Exception
	{
		char[] nums = arg1.toCharArray();
		boolean[] bits = new boolean[nums.length];
		for(int i = 0; i < nums.length; i++) {
			if(nums[nums.length-1-i] == '1') {
				bits[i] = true;
			}else if(nums[nums.length-1-i] == '0') {
				bits[i] = false;
			}else {
				throw new Exception("Please input binary numbers");
			}
		}
		return bits;
	}

	private static String bits2string(boolean[] bits)
	{
		String num = "";
		for(int i = 0; i < bits.length; i++) {
			if(bits[bits.length-1-i]) {
				num += "1";
			}else {
				num += "0";
			}
		}
		return num;
	}

	private static long bits2decimal(boolean[] bits) {
		long result = 0;
		long exp = 1;
		boolean[] bits1 =  bits.clone();
		boolean sign = bits1[bits1.length-1];
		for(int i = 0; i < bits1.length - 1; i++) {
			if(bits1[i] == true) {
				result += exp;
			}
			exp = exp*2;
		}
		if(sign) {
			result -= exp;
		}
		return result;
	}

	boolean sign(boolean[] bits) {
		return bits[bits.length-1];
	}

	private static boolean[] add(boolean[] num1, boolean[] num2) {
		boolean[] sum = new boolean[num1.length];
		boolean carry = false;
		for(int i = 0; i < sum.length; i++) {
			if(num1[i]) {
				if(num2[i]) {
					if(carry) {
						sum[i] = true;
						carry = true;
					}else {
						sum[i] = false;
						carry = true;
					}
				}else {
					if(carry) {
						sum[i] = false;
						carry = true;
					}else {
						sum[i] = true;
						carry = false;
					}
				}
			}else {
				if(num2[i]) {
					if(carry) {
						sum[i] = false;
						carry = true;
					}else {
						sum[i] = true;
						carry = false;
					}
				}else {
					if(carry) {
						sum[i] = true;
						carry = false;
					}else {
						sum[i] = false;
						carry = false;
					}
				}
			}
		}
		return sum;
	}

	private static boolean[] subtract(boolean[] minuend, boolean[] sub) {
		boolean[] min = minuend.clone();
		boolean[] difference = new boolean[min.length];
		for(int i = 0; i < difference.length; i++) {
			if(min[i] == true && sub[i] == false) {
				difference[i] = true;
			}else if(min[i] == true && sub[i] == true) {
				difference[i] = false;
			}else if(min[i] == false && sub[i] == true) {
				difference[i] = true;
				for(int j = i+1; j < min.length; j++) {
					if(min[j]) {
						min[j] = false;
						break;
					}else {
						min[j] = true;
					}
				}
			}else {
				difference[i] = false;
			}
		}
		return difference;
	}

	private static boolean[] leftShift(boolean [] num, int bits) {
		for(int i = 0; i < bits; i++) {
			boolean placeholder = num[0];
			num[0] = false;
			for(int j = 0; j < num.length-1; j++) {
				boolean val = num[j+1];
				num[j+1] = placeholder;
				placeholder = val;
			}
		}
		return num;
	}

	private static boolean[] rightShift(boolean [] num, int bits) {
		for(int i = 0; i < bits; i++) {
			boolean placeholder = num[num.length-1];
			num[num.length-1] = false;
			for(int j = num.length-1; j > 0; j--) {
				boolean val = num[j-1];
				num[j-1] = placeholder;
				placeholder = val;
			}
		}
		return num;
	}

	private static boolean[] extend(boolean[] num, int add) {
		boolean[] result = new boolean[num.length+add];
		if(num[num.length-1]) {
			for(int i = 0; i < result.length; i++) {
				if(i < num.length) {
					result[i] = num[i];
				}else {
					result[i] = true;
				}
			}
		}else {
			for(int i = 0; i < result.length; i++) {
				if(i < num.length) {
					result[i] = num[i];
				}else {
					result[i] = false;
				}
			}
		}
		return result;
	}
	
//	private static boolean[] extend(boolean[] num, int add, boolean sign) {
//		boolean[] result = new boolean[num.length+add];
//		if(sign) {
//			for(int i = 0; i < result.length; i++) {
//				if(i < num.length) {
//					result[i] = num[i];
//				}else {
//					result[i] = true;
//				}
//			}
//		}else {
//			for(int i = 0; i < result.length; i++) {
//				if(i < num.length) {
//					result[i] = num[i];
//				}else {
//					result[i] = false;
//				}
//			}
//		}
//		return result;
//	}

	private static boolean[] negate(boolean[] bits) {
		try {
			boolean[] one = string2bits("01");
			one = extend(one, bits.length-2);


			for(int i = 0; i < bits.length; i++) {
				bits[i] = !bits[i];
			}
			bits = add(bits, one);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return bits;
	}

	private static boolean[] multiply(boolean[] multiplier, boolean[] multiplicand) {
		boolean[] product = new boolean[multiplier.length*2];
		boolean[] plier = multiplier.clone();
		boolean[] cand = extend(multiplicand, multiplicand.length);
		for(int i = 0; i < plier.length; i++) {
			if(plier[0]) {
				product = add(product, cand);
			}
			cand = leftShift(cand, 1);
			plier = rightShift(plier, 1);
		}
		product = extend(product, -product.length/2);
		return product;
	}


	private static boolean[] absVal(boolean[] bits) {
		if(bits[bits.length-1]) {
			bits = negate(bits);
		}
		return bits;
	}

	private static Object[] divide(boolean[] bits1, boolean[] bits2){
		if(bits2decimal(bits2) == 0) {
			String err = "ERROR";
			return new Object[] {new boolean[0], new boolean[0], err};
		}
		boolean sign = false;
		if(bits1[bits1.length-1] != bits2[bits2.length-1]) {
			sign = true;
		}
		boolean[] remainder = bits1.clone();
		boolean[] divisor = bits2.clone();
		boolean[] quotient = new boolean[remainder.length];
		remainder = extend(remainder, remainder.length);
		divisor = extend(divisor, divisor.length);
		remainder = absVal(remainder);
		divisor = absVal(divisor);
		divisor = leftShift(divisor, divisor.length/2);
		for(int i = 0; i <= bits1.length; i++) {
			remainder = subtract(remainder, divisor);
			if(!remainder[remainder.length-1]) {
				quotient = leftShift(quotient, 1);
				quotient[0] = true;
			}else {
				remainder = add(remainder, divisor);
				quotient = leftShift(quotient, 1);
				quotient[0] = false;
			}
			divisor = rightShift(divisor, 1);
		}
		remainder = absVal(remainder);
		if(sign) {
			quotient = negate(quotient);
		}
		if(bits1[bits1.length - 1]) {
			remainder = negate(remainder);
		}
		Object[] array = new Object[2];
		array[0] = quotient;
		array[1] = extend(remainder, -remainder.length/2);
		return array;

	}


}
