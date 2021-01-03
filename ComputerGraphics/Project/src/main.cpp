#include <glad/glad.h>
#include <GLFW/glfw3.h>

#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>

#define STB_IMAGE_IMPLEMENTATION
#include <stb_image.h>

#include <iostream>
#include <math.h>

#include <shader.h>
#include <camera.h>
#include <model.h>

#include <sstream>

void framebuffer_size_callback(GLFWwindow* window, int width, int height);
void mouse_callback(GLFWwindow* window, double xpos, double ypos);
void scroll_callback(GLFWwindow* window, double xoffset, double yoffset);
void processInput(GLFWwindow *window);

const unsigned int SCR_WIDTH = 800;
const unsigned int SCR_HEIGHT = 600;

Camera camera(glm::vec3(0.0f, 0.0f, 3.0f));
float lastX = SCR_WIDTH / 2.0f;
float lastY = SCR_HEIGHT / 2.0f;
bool firstMouse = true;
   
float deltaTime = 0.0f;
float lastFrame = 0.0f;
float world_time = 0.0f;
glm::vec3 light_position_world = glm::vec3(0.0, 0.0, 2.0);
bool next_state = true;
bool draw_hitbox = true;
float next_state_cooldown = 0;
float hitbox_cooldown = 0;
unsigned int texture;

void animate_spider(MeshHierarchy&);
void crowd_iteration(vector<Model>&, Shader&);
void draw_crowd(vector<Model>&, Shader&, Camera&,Shader&);

unsigned int vbo_sphere, vao_sphere;
vector<float> sphere_vertices;

int main()
{
    glfwInit();
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

#ifdef __APPLE__
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
#endif
    GLFWwindow* window = glfwCreateWindow(SCR_WIDTH, SCR_HEIGHT, "Computer Graphics", NULL, NULL);
    if (window == NULL)
    {
        std::cout << "Failed to create GLFW window" << std::endl;
        glfwTerminate();
        return -1;
    }
    glfwMakeContextCurrent(window);
    glfwSetFramebufferSizeCallback(window, framebuffer_size_callback);
    glfwSetCursorPosCallback(window, mouse_callback);
    glfwSetScrollCallback(window, scroll_callback);
    // tell GLFW to capture our mouse
    glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_DISABLED);
    if (!gladLoadGLLoader((GLADloadproc)glfwGetProcAddress))
    {
        std::cout << "Failed to initialize GLAD" << std::endl;
        return -1;
    }
    stbi_set_flip_vertically_on_load(true);
    glEnable(GL_DEPTH_TEST);
    
    Shader sphere_shader = Shader("../shaders/ball.vert", "../shaders/ball.frag");


    ifstream in("../sphere.txt");
    std::string line;
    while (std::getline(in, line)) {
      std::istringstream iss(line);
      float a, b, c;
      if ((iss >> a >> b >> c)) {
	sphere_vertices.push_back(a);
	sphere_vertices.push_back(b);
	sphere_vertices.push_back(c);
      }
    }
    
    glGenVertexArrays(1, &vao_sphere);
    glGenBuffers(1, &vbo_sphere);
    glBindVertexArray(vao_sphere);
    glBindBuffer(GL_ARRAY_BUFFER, vbo_sphere);
    glBufferData(GL_ARRAY_BUFFER, sphere_vertices.size() * sizeof(float), &sphere_vertices[0], GL_STATIC_DRAW);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0,(void*) 0);
    glEnableVertexAttribArray(0);
    glBindBuffer(GL_ARRAY_BUFFER, 0); 
    glBindVertexArray(0); 


    vector<Model> crowd;
    int crowd_size = 4;
    Shader spider_shader("../shaders/light.vert", "../shaders/light.frag");
    char modelPath[] = "../models/spider.obj";
    char modelHierarchyPath[] = "../models/spider.json";

    for (int i = 0; i < crowd_size; ++i){
      Model spider_model(modelPath, modelHierarchyPath, true);
      spider_model.pos = glm::vec2(0, i * 4.0f);
      crowd.push_back(spider_model);
    }

    
    Shader plane_shader("../shaders/shader.vert", "../shaders/shader.frag");
    Model ball_model("../models/ball.dae");
    Shader ball_shader("../shaders/ball.vert", "../shaders/ball.frag");
    
    float vertices[] = {
      -1.0f, 0.0f, 1.0f, 0.0f, 1.0f, 0.0f, 0.0f, 0.0f,   // bottom left
      -1.0f, 0.0f, -1.0f,0.0f, 1.0f, 0.0f,  0.0f, 1.0f,  // top left
      1.0f, 0.0f, 1.0f, 0.0f, 1.0f, 0.0f, 1.0f, 0.0f,    // bottom left
      1.0f, 0.0f, -1.0f, 0.0f, 1.0f, 0.0f, 1.0f, 1.0f,    // top right
    };
    glGenTextures(1, &texture);
    glBindTexture(GL_TEXTURE_2D, texture);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_MIRRORED_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_MIRRORED_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    int width, height, nrChannels;
    unsigned char *data = stbi_load("../textures/ground.jpg", &width, &height, &nrChannels, 0);
    if (data) {
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, width, height, 0, GL_RGB, GL_UNSIGNED_BYTE, data);
	glGenerateMipmap(GL_TEXTURE_2D);
    } else std::cout << "Failed to load texture" << std::endl;
    
    stbi_image_free(data);
    unsigned int indices[] = { 0,1,2,3,1,2 };
    unsigned int VBO, VAO, EBO;
    glGenVertexArrays(1, &VAO);
    glGenBuffers(1, &VBO);
    glGenBuffers(1, &EBO);
    glBindVertexArray(VAO);
    glBindBuffer(GL_ARRAY_BUFFER, VBO);
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, EBO);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(indices), indices, GL_STATIC_DRAW);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 8 * sizeof(float), (void*)(0 * sizeof(float)));
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 8 * sizeof(float), (void*)(3 * sizeof(float)));
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, 8 * sizeof(float), (void*)(6 * sizeof(float)));
    glEnableVertexAttribArray(2);
    glBindBuffer(GL_ARRAY_BUFFER, 0); 
    glBindVertexArray(0); 

    
    while (!glfwWindowShouldClose(window)) {
      float currentFrame = glfwGetTime();
      deltaTime = currentFrame - lastFrame;
      lastFrame = currentFrame;
      world_time += deltaTime;
      next_state_cooldown = 0 > next_state_cooldown - deltaTime ? 0 : next_state_cooldown - deltaTime;
      hitbox_cooldown = 0 > hitbox_cooldown - deltaTime ? 0 : hitbox_cooldown - deltaTime;
      processInput(window);
      
      glClearColor(0.05f, 0.05f, 0.05f, 1.0f);
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
      // camera uniforms 
      glm::mat4 projection = glm::perspective(glm::radians(camera.Zoom), (float)SCR_WIDTH / (float)SCR_HEIGHT, 0.1f, 100.0f);
      glm::mat4 view = camera.GetViewMatrix();
      spider_shader.use();
      spider_shader.setMat4("projection", projection);
      spider_shader.setMat4("view", view);
      // light uniforms
      glm::vec3 view_position = camera.GetViewPosition();
      spider_shader.setVec3("view_position", view_position);
      spider_shader.setVec3("light_position", light_position_world);

      if (next_state){
	crowd_iteration(crowd, spider_shader);
      }
      draw_crowd(crowd, spider_shader,camera, sphere_shader);
      
      ball_shader.use();
      ball_shader.setMat4("projection", projection);
      ball_shader.setMat4("view", view);
      glm::mat4 model = glm::mat4(1.0f);
      model = glm::translate(model, light_position_world);
      model = glm::scale(model, glm::vec3(0.2f));
      ball_model.hierarchy.setTransform("root", model);
      ball_model.hierarchy.compileTransforms();
      ball_model.Draw(ball_shader);
      ball_model.hierarchy.resetTransforms();
      
      plane_shader.use();
      glBindTexture(GL_TEXTURE_2D, texture);
      plane_shader.setVec3("view_position", view_position);
      plane_shader.setVec3("light_position", light_position_world);
      plane_shader.setMat4("projection", projection);
      plane_shader.setMat4("view", view);
      plane_shader.setMat4("model",
			   glm::scale(glm::translate(glm::mat4(1.0f), glm::vec3(0.0f, -0.24f, 0.0f)),
				      glm::vec3(10.0f, 10.0f, 10.0f)));
      glBindVertexArray(VAO);
      glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);

      // glfw: swap buffers and poll IO events (keys pressed/released, mouse moved etc.)
      glfwSwapBuffers(window);
      glfwPollEvents();
    }

    // glfw: terminate, clearing all previously allocated GLFW resources.
    glfwTerminate();
    return 0;
}


// process all input: query GLFW whether relevant keys are pressed/released this frame and react accordingly
void processInput(GLFWwindow *window)
{
  if (glfwGetKey(window, GLFW_KEY_ESCAPE) == GLFW_PRESS)
    glfwSetWindowShouldClose(window, true);
  if (glfwGetKey(window, GLFW_KEY_W) == GLFW_PRESS)
    camera.ProcessKeyboard(FORWARD, deltaTime);
  if (glfwGetKey(window, GLFW_KEY_S) == GLFW_PRESS)
    camera.ProcessKeyboard(BACKWARD, deltaTime);
  if (glfwGetKey(window, GLFW_KEY_A) == GLFW_PRESS)
    camera.ProcessKeyboard(LEFT, deltaTime);
  if (glfwGetKey(window, GLFW_KEY_D) == GLFW_PRESS)
    camera.ProcessKeyboard(RIGHT, deltaTime);

  if (glfwGetKey(window, GLFW_KEY_T) == GLFW_PRESS && next_state_cooldown == 0) { 
    next_state = !next_state;
    next_state_cooldown = 0.1;
    cout << "PAUSE/PLAY" << endl;
  }
  if (glfwGetKey(window, GLFW_KEY_G) == GLFW_PRESS && hitbox_cooldown == 0) { 
    draw_hitbox = !draw_hitbox;
    hitbox_cooldown = 0.1;
  }
  
  float speed = 0.1f;
  if (glfwGetKey(window, GLFW_KEY_I) == GLFW_PRESS){
    light_position_world.z = light_position_world.z - speed;
    light_position_world.z = light_position_world.z - speed;
  }      
  if (glfwGetKey(window, GLFW_KEY_K) == GLFW_PRESS){
    light_position_world.z = light_position_world.z + speed;
    light_position_world.z = light_position_world.z + speed;
  }
  if (glfwGetKey(window, GLFW_KEY_J) == GLFW_PRESS){
    light_position_world.x = light_position_world.x - speed;
    light_position_world.x = light_position_world.x - speed;
  }
  if (glfwGetKey(window, GLFW_KEY_L) == GLFW_PRESS){
    light_position_world.x = light_position_world.x + speed;
    light_position_world.x = light_position_world.x + speed;
  }

}

// glfw: whenever the window size changed (by OS or user resize) this callback function executes
void framebuffer_size_callback(GLFWwindow* window, int width, int height)
{
    // make sure the viewport matches the new window dimensions; note that width and 
    // height will be significantly larger than specified on retina displays.
    glViewport(0, 0, width, height);
}

// glfw: whenever the mouse moves, this callback is called
void mouse_callback(GLFWwindow* window, double xpos, double ypos)
{
    if (firstMouse)
    {
        lastX = xpos;
        lastY = ypos;
        firstMouse = false;
    }
    float xoffset = xpos - lastX;
    float yoffset = lastY - ypos; // reversed since y-coordinates go from bottom to top
    lastX = xpos;
    lastY = ypos;
    camera.ProcessMouseMovement(xoffset, yoffset);
}

// glfw: whenever the mouse scroll wheel scrolls, this callback is called
// ----------------------------------------------------------------------
void scroll_callback(GLFWwindow* window, double xoffset, double yoffset)
{
    camera.ProcessMouseScroll(yoffset);
}

void animate_spider(MeshHierarchy &model_hierarchy){
  glm::mat4 legs1_model = glm::mat4(1.0f);
  legs1_model = glm::rotate(legs1_model, sin(world_time * 20) * 0.05f, glm::vec3(0.0, 1.0, 0.0));
  model_hierarchy.addRotation("set1", legs1_model);
    
  glm::mat4 legs2_model = glm::mat4(1.0f);
  legs2_model = glm::rotate(legs2_model, cos(world_time * 20) * 0.05f, glm::vec3(0.0, 1.0, 0.0));
  model_hierarchy.addRotation("set2", legs2_model);    
}



void crowd_iteration(vector<Model>& crowd, Shader& spider_shader) {
  for (int i = 0; i < crowd.size(); i++){
    auto& spider_model = crowd[i];
    // for (int j = 0; j < crowd.size(); j++){
    //   if (i != j) {
    // 	auto& other_spider = crowd[j];
    // 	//	sum_dist += glm::distance(spider_model.pos, other_spider.pos);
    // 	if (glm::distance(spider_model.pos, other_spider.pos) < 2){
    // 	}
    //   }
    // }
    float rotation = i % 2 == 0 ? -0.01 : 0.01;
    spider_model.rotate(rotation * (i + 1));
    spider_model.move_forward(0.055 * i + 0.01);
    animate_spider(spider_model.hierarchy);
  }

}



void draw_crowd(vector<Model>& crowd, Shader& spider_shader, Camera& cam, Shader& sphere_shader) {
  auto model = glm::mat4(1.0f);
  auto scale_matrix = glm::vec3(0.01f, 0.01f, 0.01f);
  glm::mat4 projection = glm::perspective(glm::radians(cam.Zoom), (float)SCR_WIDTH / (float)SCR_HEIGHT, 0.1f, 100.0f);
  glm::mat4 view = cam.GetViewMatrix();

  // auto normalised_model = glm::scale(model, scale_matrix);
  for (auto& spider : crowd) {
    // the order of these transformations are backwards!!!! 
    auto translation = glm::translate(glm::mat4(1.0f), glm::vec3(-spider.pos.x, 0, -spider.pos.y));
    model = glm::rotate(translation, -spider.forward_direction_rad, glm::vec3(0.0f, 1.0f, 0.0f));
    model = glm::scale(model, scale_matrix);
    spider.hierarchy.setTransform("root", model); 
    spider.hierarchy.compileTransforms();
    spider_shader.use();
    spider_shader.setMat4("projection", projection);
    spider_shader.setMat4("view", view);
    spider.Draw(spider_shader);
    spider.hierarchy.resetTransforms();
    
    if (draw_hitbox) {
      sphere_shader.use();
      sphere_shader.setMat4("projection", projection);
      sphere_shader.setMat4("view", view);
      sphere_shader.setMat4("model", translation);
      glBindVertexArray(vao_sphere);
      glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
      glDrawArrays(GL_TRIANGLES, 0, 128 * 3);
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    }
  }
}

