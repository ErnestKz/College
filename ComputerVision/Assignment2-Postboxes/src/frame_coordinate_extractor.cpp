#include <opencv2/opencv.hpp>
#include <iostream>

using namespace std;
using namespace cv;

int FRAME_SCALE = 2;

int num_clicked = 0;

void mouse_event(int evt, int x, int y, int flags, void *param){
  Mat* rgb = (Mat*) param;
  if (evt == EVENT_LBUTTONDOWN) {

    string string = "";

    
    if (num_clicked % 4 == 0)
      string = "},\n{";
    else
      string =  ", ";


    string += to_string(x/FRAME_SCALE) + "," +  to_string(y/FRAME_SCALE);
    cout << string << endl;
    num_clicked++;
  }   
}



int main(int arg, char *argv[]){
  int selected_frame = stoi(argv[1]);

  int current_frame = 0;
    
  VideoCapture cap("../PostboxesWithLines.avi");
  if (!cap.isOpened()){
    cout<< "Error opening video" << endl;
    return -1;
  }

  Mat frame;
  char key = 0;
  cap >> frame;
  while (!frame.empty()){
    
    if ( current_frame == selected_frame ){
      string frame_name = "Frame " + to_string(selected_frame);
      
      namedWindow(frame_name);
      setMouseCallback(frame_name, mouse_event, &frame);
      resize(frame,frame, Size(), FRAME_SCALE, FRAME_SCALE);              
      while ((int) key != 27 ) {
	imshow(frame_name, frame);
	key = waitKey(1);
      }

    }
    
    cap >> frame;
    ++current_frame;
  }
  
  cap.release();
  
  return 0;
}
