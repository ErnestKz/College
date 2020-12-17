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



Point2f* postbox_locations[NUMBER_OF_POSTBOXES];
Point2f* edge_locations[NUM_EDGES];
Point2f destination_points [4];
Point2f destination_points_edges [4];
 
void init(){
  for ( int i = 0; i < NUMBER_OF_POSTBOXES; ++i )
    postbox_locations[i] = postbox_location_points(PostboxLocations[i]);
  for ( int i = 0; i < NUM_EDGES; ++i )
    edge_locations[i] = postbox_location_points(EdgeLocations[i]);
  
  destination_points[0] = {0, 0};
  destination_points[1] = {50, 0};
  destination_points[2] = {0, 50};
  destination_points[3] = {50, 50};
  
  destination_points_edges[0] = {0, 0};
  destination_points_edges[1] = {25, 0};
  destination_points_edges[2] = {0, 100};
  destination_points_edges[3] = {25, 100};
}



vector<Mat> load_frames(const char* video_path){
  VideoCapture cap(video_path);
  if (!cap.isOpened()){
    cout<< "Error opening video" << endl;
  }
  
  vector<Mat> frames;
  Mat frame;
  do {
    cap >> frame;
    frames.push_back(frame.clone());
  } while (!frame.empty());

  return frames;
}


const int LEFT_ARROW = 81;
const int UP_ARROW = 82;
const int RIGHT_ARROW = 83;
const int DOWN_ARROW = 84;
const int ESCAPE = 27;

const int KEY_Q = 113;
const int KEY_W = 119;

const int KEY_A = 97;
const int KEY_S = 115;

const int KEY_P = 112;



int modulo_loop(int x, int m){
  return ( x % m + m) % m;
}

Mat pipeline(int stage, Mat frame, int binary_thresh, int binary_edge_thresh, int subsection){
  if (subsection > 0 && subsection <= NUMBER_OF_POSTBOXES){
    frame = ptf(frame, postbox_locations[subsection-1], destination_points, Size{50, 50});
    resize(frame,frame, Size(), 3.0, 3.0);    
  } else if (subsection > NUMBER_OF_POSTBOXES) {
    frame = ptf(frame, edge_locations[subsection - NUMBER_OF_POSTBOXES], destination_points_edges, Size{25,100});
    resize(frame,frame, Size(), 3.0, 3.0);        
  } else {
    resize(frame,frame, Size(), 2.0, 2.0);    
  }
  
  Mat gray_image_postbox;
  cvtColor(frame, gray_image_postbox, COLOR_BGR2GRAY);
  
  Mat binary_image_postbox;
  threshold(gray_image_postbox, binary_image_postbox, binary_thresh, 255, THRESH_BINARY);
  Mat horizontal_derivative_postbox;
  Sobel(binary_image_postbox, horizontal_derivative_postbox, CV_32F, 1, 0);
  Mat binary_edge_image_postbox;
  threshold(horizontal_derivative_postbox, binary_edge_image_postbox, binary_edge_thresh, 255, THRESH_BINARY);

  binary_edge_image_postbox.convertTo(binary_edge_image_postbox, CV_8U);
  vector<Vec4i> linesP_postbox;
  HoughLinesP(binary_edge_image_postbox, linesP_postbox, 1, CV_PI/180, 24, 20, 10 );

  Mat frame_lines = frame.clone();
  for( size_t i = 0; i < linesP_postbox.size(); i++ )
    {
      Vec4i l = linesP_postbox[i];
      line( frame_lines, Point(l[0], l[1]), Point(l[2], l[3]), Scalar(0,0,255), 1, LINE_AA);
    }

  
  switch(stage){
  case 0:
    return frame;
  case 1:
    return gray_image_postbox.clone();
  case 2:
    return binary_image_postbox.clone();
  case 3:
    return binary_edge_image_postbox.clone();
  case 4:
    return frame_lines.clone();
  default:
    return frame;
  }
}

int main(){
  init();
  auto frames = load_frames("../PostboxesWithLines.avi");
  const int num_frames = frames.size();
  const int num_stages = 5;

  int sub_section = 0;
  int selected_stage = 0;
  int selected_frame_index = 0;
  bool exit = false;

  int binary_thresh = 20;
  int binary_edge_thresh = 60;

  while (!exit) {
    char key = 0;

    string frame_name = "Frame";
    Mat selected_frame = frames[selected_frame_index];

    selected_frame = pipeline(selected_stage, selected_frame, binary_thresh, binary_edge_thresh, sub_section);
    
    imshow(frame_name, selected_frame);
    key = waitKey(300);
    

    //    cout <<(int) key << endl;
    switch((int) key){
    case RIGHT_ARROW:
      selected_frame_index = modulo_loop(selected_frame_index + 1, num_frames -1);
      cout << "Showing frame " << selected_frame_index << endl;
      break;
    case LEFT_ARROW:
      selected_frame_index = modulo_loop(selected_frame_index - 1, num_frames -1 );
      cout << "Showing frame " << selected_frame_index << endl;
      break;
    case UP_ARROW:
      selected_stage = modulo_loop(selected_stage + 1, num_stages );
      cout << "Showing stage " << selected_stage << endl;
      break;
    case DOWN_ARROW:
      selected_stage = modulo_loop(selected_stage - 1, num_stages );
      cout << "Showing stage " << selected_stage << endl;
      break;
    case KEY_Q:
      binary_thresh = modulo_loop(binary_thresh + 1, 255 -1);
      cout << "Binary thresh " << binary_thresh << endl;
      break;
    case KEY_W:
      binary_thresh = modulo_loop(binary_thresh - 1, 255 -1 );
      cout << "Binary thresh " << binary_thresh << endl;
      break;

      
    case KEY_A:
      sub_section = modulo_loop(sub_section + 1, NUMBER_OF_POSTBOXES + NUM_EDGES);
      cout << "Sub section " << sub_section << endl;
      break;
    case KEY_S:
      sub_section = modulo_loop(sub_section - 1, NUMBER_OF_POSTBOXES + NUM_EDGES);
      cout << "Sub section " << sub_section << endl;
      break;
    case KEY_P:
      imwrite("../png/frame" +
	      to_string(selected_frame_index) +
	      "_stage" + to_string(selected_stage) +
	      "_subsection" + to_string(sub_section) +".png"
	      , selected_frame);
      cout << "Png captured" << endl;
      break;
    case ESCAPE:
      exit = true;
    default:
      break;
    }
  }
  
  
  return 0;
}















