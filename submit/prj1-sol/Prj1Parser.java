import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Prj1Parser {

    public static List<String> ip = new ArrayList<>();
    public static List<String> op = new ArrayList<>();
    public static int i=0;
    public static String expr, cmplxtr;

    public static void main(String[] abc) {

        Scanner pip = new Scanner(System.in);
        expr = pip.nextLine();
        ip = inspect(expr);

        val();
    }

    public static List<String> inspect(String s) {

        List<String> inspStr = new ArrayList<>();
        Matcher insp = Pattern.compile("\\d+|\\.{3}|\\S").matcher(s);

        while (insp.find()) {
            inspStr.add(insp.group());
        }

        return inspStr;
    }

    public static void val() {

    if ((expr.matches("^\\d+$")))
        System.out.println(expr);
    else if (ip.get(i).equals("{") || (ip.get(i).equals("}"))){
        match();
        getInitializers();
        }
    else {
        System.err.println("error: syntax error got " +ip.get(i));
        System.exit(1);
    }

    }

    public static void getInitializers(){

        getInitializer();
        if (((ip.get(i)).equals(","))){
            match();
            if (((ip.get(i)).equals(","))){
                System.err.println("error: expecting '}' but got '" + ip.get(i)+"'");
                System.exit(1);
            }
            if (((ip.get(i)).equals("{"))){
                match();
                cmplxtr=complex();
                op.add(cmplxtr);
                match();
            }

            getInitializers();

        } else {
                val();
        }
    }

    public static void getInitializer() {

        int indx1=0, indx2=0, val;

    if (((ip.get(i)).equals("["))) {
        match();
        if ((ip.get(i)).matches("\\d+")) {
            indx1 = Integer.parseInt(ip.get(i));
            match();
        } else {
            System.err.println("error: expecting index but got "+ip.get(i));
            System.exit(1);
        }

        if (((ip.get(i)).equals("..."))) {
            match();
            if ((ip.get(i)).matches("\\d+")) {
                indx2 = Integer.parseInt(ip.get(i));
                match();
            }
            else {
                System.err.println("error: expecting index but got "+ip.get(i));
                System.exit(1);
            }
         }
        if (((ip.get(i)).equals("]"))) {
             match();
          }
        else {
                System.err.println("error: expecting ']' but got "+ip.get(i));
                System.exit(1);
         }

         if (((ip.get(i)).equals("="))) {
             match();
             if ((ip.get(i)).matches("\\d+")) {
                 val = Integer.parseInt(ip.get(i));
                 if (indx1>0 && indx2>0){

                     for(int j=op.size(); j<indx1; j++){
                         op.add("0");
                     }

                     try {
                         while (indx1 <= indx2) {
                             op.set(indx1, String.valueOf(val));
                             indx1++;
                         }
                     }
                     catch (Exception e){
                         while (indx1 <= indx2) {
                             op.add(indx1, String.valueOf(val));
                             indx1++;
                         }
                     }
                 } else if (indx1>0) {

                     for(int j=op.size(); j<indx1; j++){
                         op.add("0");
                     }
                     try {
                         op.set(indx1, String.valueOf(val));
                     }
                     catch(Exception e) {
                         op.add(indx1, String.valueOf(val));
                     }
                 }
                 match();
                }
             else if (ip.get(i).equals("{")) {
                 match();
                 cmplxtr = complex();
                 match();
                 if (indx1>0 && indx2>0){

                     for(int j=op.size(); j<indx1; j++){
                         op.add("0");
                     }
                     try {
                         while (indx1 <= indx2) {
                             op.set(indx1, cmplxtr);
                             indx1++;
                         }
                     }
                     catch (Exception e){
                         while (indx1 <= indx2) {
                             op.add(indx1, cmplxtr);
                             indx1++;
                         }
                     }
                 }
             }
             else {
                  System.err.println("error: Expecting value or nested array but got "+ip.get(i));
                  System.exit(1);
                }
            }
        }
        else if (((ip.get(i)).matches("\\d+"))){
                op.add(ip.get(i));
                match();
        }
    }

    public static void match(){
        String output;
        i++;
        if (i == ip.size()){
            long cnto = (ip.toString()).chars().filter(ch -> ch == '{').count();

            long cntc = (ip.toString()).chars().filter(ch -> ch == '}').count();

            if ((ip.get(i-1)).equals("}") && (cntc == cnto)) {
                output = op.toString();
                output = output.replace(" ","");
                System.out.println(output);
                System.exit(0);
            } else if (((ip.get(i-1)).equals("}") && (ip.get(i-2)).equals("}"))) {
                System.err.println("error: expecting 'EOF' but got '}'");
                System.exit(1);

            } else {
                System.err.println("error: expecting '}' but got 'EOF'");
                System.exit(1);
            }
        }
    }
    public static String complex(){
        String nestr;
        List<String> cmplx = new LinkedList<>();

        while (!ip.get(i).equals("}")){
            int cind=0, cval=0;
            if (ip.get(i).equals("[")) {
                match();
                if ((ip.get(i)).matches("\\d+")) {
                    cind = Integer.parseInt(ip.get(i));
                    match();
                } else
                    System.err.println("Error" +ip.get(i));
                if (ip.get(i).equals("]"))
                    match();
                else System.err.println("Error");

                if (ip.get(i).equals("=")) {
                    match();
                    if (ip.get(i).matches("\\d+")) {
                        cval = Integer.parseInt(ip.get(i));
                        match();
                    }
                }
            }
            else if ((ip.get(i)).matches("\\d+")) {
                cmplx.add(ip.get(i));
                match();
            }
            else match();

            if ((cind > 0) && (cval >0)) {
                for (int k = cmplx.size(); k < cind; k++) {
                    cmplx.add("0");
                }
                cmplx.add(cind, String.valueOf(cval));
            }
        }
        nestr = cmplx.toString();
        return nestr;
    }
}

