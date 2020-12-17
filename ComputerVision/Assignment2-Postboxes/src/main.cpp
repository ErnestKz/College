#include <opencv2/opencv.hpp>
#include <iostream>

using namespace std;
using namespace cv;

#define NUMBER_OF_POSTBOXES 6
int PostboxLocations[NUMBER_OF_POSTBOXES][8] = {
	{ 26, 113, 106, 113, 13, 133, 107, 134 },
	{ 119, 115, 199, 115, 119, 135, 210, 136 },
	{ 30, 218, 108, 218, 18, 255, 109, 254 },
	{ 119, 217, 194, 217, 118, 253, 207, 253 },
	{ 32, 317, 106, 315, 22, 365, 108, 363 },
	{ 119, 315, 191, 314, 118, 362, 202, 361 } };
#define POSTBOX_TOP_LEFT_COLUMN 0
#define POSTBOX_TOP_LEFT_ROW 1
#define POSTBOX_TOP_RIGHT_COLUMN 2
#define POSTBOX_TOP_RIGHT_ROW 3
#define POSTBOX_BOTTOM_LEFT_COLUMN 4
#define POSTBOX_BOTTOM_LEFT_ROW 5
#define POSTBOX_BOTTOM_RIGHT_COLUMN 6
#define POSTBOX_BOTTOM_RIGHT_ROW 7

#define NUM_EDGES 9

int EdgeLocations[NUM_EDGES][8]=
  {{1,83
    , 14,85
    , 1,132
    , 14,133
    },
   {106,20
    , 121,22
    , 106,133
    , 118,134
   },
   {213,24
    , 222,26
    , 207,131
    , 214,132
   },
   {3,151
    , 20,151
    , 7,251
    , 20,251
   },
   {106,154
    , 120,153
    , 107,250
    , 117,250
   },
   {205,153
    , 220,155
    , 203,248
    , 214,250
   },
   {7,275
    , 28,275
    , 9,363
    , 22,361
   },
   {109,271
    , 123,270
    , 106,358
    , 119,358
   },
   {201,270
    , 215,271
    , 198,353
    , 211,355}};

  
// perspective transformed frame
Mat ptf(Mat orig_frame, Point2f source_points[], Point2f destination_points[], Size s){
  Mat perspective_matrix = getPerspectiveTransform(source_points, destination_points);
  Mat pt_frame;
  warpPerspective( orig_frame, pt_frame, perspective_matrix, s);
  return pt_frame;
}


Point2f* postbox_location_points(int *postbox){
  Point2f* source_points = new Point2f[4];
  source_points[0] = { postbox[0], postbox[1] };
  source_points[1] = { postbox[2], postbox[3] };
  source_points[2] = { postbox[4], postbox[5] };
  source_points[3] = { postbox[6], postbox[7] };
  return source_points;
}



int main(){

  Point2f* postbox_locations[NUMBER_OF_POSTBOXES];
  for ( int i = 0; i < NUMBER_OF_POSTBOXES; ++i )
    postbox_locations[i] = postbox_location_points(PostboxLocations[i]);

  Point2f* edge_locations[NUM_EDGES];
  for ( int i = 0; i < NUM_EDGES; ++i )
    edge_locations[i] = postbox_location_points(EdgeLocations[i]);

  
  Point2f destination_points [4];
  destination_points[0] = {0, 0};
  destination_points[1] = {50, 0};
  destination_points[2] = {0, 50};
  destination_points[3] = {50, 50};

  Point2f destination_points_edges [4];
  destination_points_edges[0] = {0, 0};
  destination_points_edges[1] = {25, 0};
  destination_points_edges[2] = {0, 100};
  destination_points_edges[3] = {25, 100};





  Size box_size {50,50};
  
  VideoCapture cap("../PostboxesWithLines.avi");
  if (!cap.isOpened()){
    cout<< "Error opening video" << endl;
    return -1;
  }

  Mat frame;
  int frameno = 0;
  char esc = 0;
  cap >> frame;

  string frame_status;

  while (!frame.empty() && esc != 27){

    string box_status = "";
    for ( int i = 0; i < NUMBER_OF_POSTBOXES; ++i ){
      Mat box = ptf(frame, postbox_locations[i], destination_points, box_size);
      // turn into grayscale
      Mat gray_image_postbox;
      cvtColor(box, gray_image_postbox, COLOR_BGR2GRAY);
      // threshold grayscale image
      Mat binary_image_postbox;
      threshold(gray_image_postbox, binary_image_postbox, 20, 255, THRESH_BINARY);
      // detect vertical edges
      Mat horizontal_derivative_postbox;
      Sobel(binary_image_postbox, horizontal_derivative_postbox, CV_32F, 1, 0);
      // threshold the gradient edge image
      Mat binary_edge_image_postbox;
      threshold(horizontal_derivative_postbox, binary_edge_image_postbox, 60, 255, THRESH_BINARY);
      // hough transform ont he binary edge image
      binary_edge_image_postbox.convertTo(binary_edge_image_postbox, CV_8U);
      vector<Vec4i> linesP_postbox;
      HoughLinesP(binary_edge_image_postbox, linesP_postbox, 1, CV_PI/180, 24, 20, 10 );
      for( size_t i = 0; i < linesP_postbox.size(); i++ )
	{
	  Vec4i l = linesP_postbox[i];
	  line( box, Point(l[0], l[1]), Point(l[2], l[3]), Scalar(0,0,255), 1, LINE_AA);
	}
      // count 'vertical' lines
      int vertical_lines = 0;
      int MIN_DISTANCE = 4;
      int prev_x_pos = 10000;
      for( size_t i = 0; i < linesP_postbox.size(); i++ )
	{
	  Vec4i l = linesP_postbox[i];
	  float bottom_x;
	  if (l[1] > l[3])
	    bottom_x = l[0];
	  else
	    bottom_x = l[2];
	  if (abs(bottom_x - prev_x_pos) > MIN_DISTANCE)
	    ++vertical_lines;
	  prev_x_pos = bottom_x;
	}
      //      resize(box,box, Size(), 3.0, 3.0);    
      imshow("box" + to_string(i), binary_edge_image_postbox);
      resize(box,box, Size(), 3.0, 3.0);    
      imshow("box" + to_string(i), box);
      if (vertical_lines <= 4)
	box_status += to_string(i+1) + " ";
    }



    int num_edges_seen = 0;
    for ( int i = 0; i < NUM_EDGES; ++i ){
      Mat box = ptf(frame, edge_locations[i], destination_points_edges, Size{25,100});
      Mat gray_image_postbox;
      cvtColor(box, gray_image_postbox, COLOR_BGR2GRAY);
      Mat binary_image_postbox;
      threshold(gray_image_postbox, binary_image_postbox, 150, 255, THRESH_BINARY);
      Mat horizontal_derivative_postbox;
      Sobel(binary_image_postbox, horizontal_derivative_postbox, CV_32F, 1, 0);
      Mat binary_edge_image_postbox;
      threshold(horizontal_derivative_postbox, binary_edge_image_postbox, 10, 255, THRESH_BINARY);
      binary_edge_image_postbox.convertTo(binary_edge_image_postbox, CV_8U);
      vector<Vec4i> linesP_postbox;
      HoughLinesP(binary_edge_image_postbox, linesP_postbox, 1, CV_PI/180, 24, 20, 10 );
      for( size_t i = 0; i < linesP_postbox.size(); i++ )
	{
	  Vec4i l = linesP_postbox[i];
	  line( box, Point(l[0], l[1]), Point(l[2], l[3]), Scalar(0,0,255), 1, LINE_AA);
	}
      if ( linesP_postbox.size() >= 1)
	num_edges_seen++;
      resize(binary_edge_image_postbox,binary_edge_image_postbox, Size(), 3.0, 3.0);    
      imshow("edge" + to_string(i), binary_edge_image_postbox);
      resize(box,box, Size(), 3.0, 3.0);    
      imshow("edge" + to_string(i), box);
    }
    
    if (box_status == ""){
      frame_status = to_string(frameno + 1) + ", No post";
    } else {
      frame_status = to_string(frameno + 1) + ", Post in " + box_status;
    } 
    if (num_edges_seen <= 8)
      frame_status = to_string(frameno + 1) + ", View obscured";
    cout << frame_status << endl;

    
    char esc = (char)waitKey(1);
    cap >> frame;
    ++frameno;
  }
  
  cap.release();
  return 0;
}
