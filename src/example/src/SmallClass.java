public class SmallClass{
    private static int COUNTER = 0;
    public static int getNext(){
        return COUNTER++;
    }
}