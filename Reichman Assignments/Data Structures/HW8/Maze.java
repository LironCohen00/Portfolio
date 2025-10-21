

import java.util.ArrayList; 
import java.awt.Color; 
 
/** 
 * A class for decomposing images into connected components. 
 */ 
public class Maze { 
 
   private UnionFind uf; 
   private DisplayImage image; 
   private int startX;   // x-coordinate for the start pixel
   private int startY;	 // y-coordinate for the start pixel
   private int endX;	 // x-coordinate for the end pixel
   private int endY;	 // y-coordinate for the end pixel
  
  /** 
    * Constructor - reads image, and decomposes it into its connected components.
    * After calling the constructor, the mazeHasSolution() and areConnected() methods
	* are expect to return a correct solution.
    * 
    * @param fileName name of image file to process. 
    */ 
   public Maze (String fileName, Color c) { 
		image = new DisplayImage(fileName);
      uf = new UnionFind(image.width() * image.height());
      int countRed = 0;
      for (int i = 0; i < image.width() ; i++) {
         for (int j = 0; j < image.height(); j++) {
            if (image.isRed(i, j) == true) { //checking for start and end coordinates
               if (countRed == 0) { // first occurence of red pixel
                  startX = i;
                  startY = j;
                  countRed++;
               }else{ // second occurence of red pixel
                  endX = i;
                  endY = j;  
               }
               image.set(i, j, c);
            } 
            if (j != image.height() - 1){ // its not the bottom row of pixels
               connect(i, j + 1, i, j); // connect pixel with the pixel to its right
            }
            if (i != image.width() - 1) { // its not the right most column of pixel
               connect(i + 1, j, i, j); // connect pixel with the pixel below it
            }
         }
      }
   } 
 
   /** 
    * Generates a unique integer id from (x, y) coordinates. 
    * It is suggested you implement this function, in order to transform
	* pixels into valid indices for the UnionFind data structure.
    *
	* @param x x-coordinate. 
    * @param y y-coordinate. 
    * @return unique id. 
    */ 
   private int pixelToId (int x, int y) { 
        return (y*image.width() + x);
   } 
 
   /** 
    * Connects two pixels if they both belong to the same image 
    * area (background or foreground), and are not already connected. 
    * It is assumed that the pixels are neighbours.
    * 
    * @param x1 x-coordinate of first pixel. 
    * @param y1 y-coordinate of first pixel. 
    * @param x2 x-coordinate of second pixel. 
    * @param y2 y-coordinate of second pixel. 
    */ 
   public void connect (int x1, int y1, int x2, int y2) { 
      if ( (image.isOn(x1,y1) && image.isOn(x2, y2)) | (!image.isOn(x1,y1) && !image.isOn(x2, y2))) {
         int rep1 = uf.find(pixelToId(x1, y1) + 1); // plus one since uf starts at index 1
         int rep2 = uf.find(pixelToId(x2, y2) + 1); 
         if (rep1 != rep2) { // if they arent already in the same set
            uf.union(rep1,rep2);
         }   
      }
   } 
 

 
   /** 
    * Checks if two pixels are connected (belong to the same component). 
    * 
    * @param x1 x-coordinate of first pixel. 
    * @param y1 y-coordinate of first pixel. 
    * @param x2 x-coordinate of second pixel. 
    * @param y2 y-coordinate of second pixel. 
    * @return true if the pixels are connected, false otherwise. 
    */ 
   public boolean areConnected (int x1, int y1, int x2, int y2) { 
      int rep1 = uf.find(pixelToId(x1, y1) + 1); // plus one since uf starts at index 1
      int rep2 = uf.find(pixelToId(x2, y2) + 1); 
      if (rep1 == rep2) { // if they arent already in the same set
         return true;
      } 
		return false;
   } 
 
   /** 
    * Finds the number of components in the image. 
    * 
    * @return the number of components in the image 
    */ 
   public int getNumComponents() { 
		return uf.getNumSets();
   } 
 
   /**
    * Returns true if and only if the maze has a solution.
    * In this case, the start and end pixel will belong to the same connected component.
    * 
    * @return true if and only if the maze has a solution
    */
   public boolean mazeHasSolution(){
		return (areConnected(startX, startY, endX, endY));
   }
      
   /** 
    * Creates a visual representation of the connected components. 
    * 
    * @return a new image with each component colored in a random color. 
    */ 
   public DisplayImage getComponentImage() { 
 
      DisplayImage compImg = new DisplayImage (image.width(), image.height()); 
      ArrayList<Integer> usedIds = new ArrayList<Integer>(); 
      int numComponents = getNumComponents(); 
      final int MAX_COLOR = 0xffffff; 
      Color colors[] = new Color[numComponents]; 
 
      colors[0] = new Color (0, 0, 100); 
      for (int c = 1; c < numComponents; c++) 
         colors[c] = new Color ((int)(Math.random() * MAX_COLOR)); 
 
      for (int x = 0; x < image.width(); x++) 
         for (int y = 0; y < image.height(); y++) { 
            
            int componentId = uf.find (pixelToId (x, y) + 1); 
            // Check if this id already exists (inefficient for a large number of components). 
            int index = -1; 
            for (int i = 0; i < usedIds.size(); i++) {
               if (usedIds.get (i) == componentId) {
                  index = i; 
               }
            }
            if (index == -1) { 
               usedIds.add (componentId); 
               index = usedIds.size() - 1; 
            } 
            compImg.set (x, y, colors[index]); 
         } 
 
      return compImg; 
   } 
 
   /** 
    * Various tests for the segmentation functionality. 
    * 
    * @param args command line arguments - name of file to process. 
    */ 
   public static void main (String[] args) { 
 
	  Maze maze = new Maze (args[0], Color.black); 
 
     System.out.println (maze.mazeHasSolution());
 
     System.out.println ("Number of components: " + maze.getNumComponents()); 
 
     DisplayImage compImg = maze.getComponentImage(); 
     compImg.show(); 
   } 
} 
