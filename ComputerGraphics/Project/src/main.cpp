#include <glad/glad.h>
#include <GLFW/glfw3.h>

#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>

#define STB_IMAGE_IMPLEMENTATION
#include <stb_image.h>

#include <shader.h>
#include <camera.h>
#include <model.h>

#include <iostream>
#include <math.h>

void framebuffer_size_callback(GLFWwindow* window, int width, int height);
void mouse_callback(GLFWwindow* window, double xpos, double ypos);
void scroll_callback(GLFWwindow* window, double xoffset, double yoffset);
void processInput(GLFWwindow *window);

// settings
const unsigned int SCR_WIDTH = 800;
const unsigned int SCR_HEIGHT = 600;

// camera
Camera camera(glm::vec3(0.0f, 0.0f, 3.0f));
float lastX = SCR_WIDTH / 2.0f;
float lastY = SCR_HEIGHT / 2.0f;
bool firstMouse = true;

// timing
float deltaTime = 0.0f;
float lastFrame = 0.0f;

float world_time = 0.0f;

glm::vec3 light_position_world = glm::vec3(0.0, 0.0, 2.0);

unsigned int texture;

// spider animate
void spiderAnimate(MeshHierarchy&);
void crowd_iteration(vector<Model>&, Shader&);
int main()
{
    // glfw: initialize and configure
    // ------------------------------
    glfwInit();
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

#ifdef __APPLE__
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
#endif

    // glfw window creation
    // --------------------
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

    // glad: load all OpenGL function pointers
    // ---------------------------------------
    if (!gladLoadGLLoader((GLADloadproc)glfwGetProcAddress))
    {
        std::cout << "Failed to initialize GLAD" << std::endl;
        return -1;
    }

    // tell stb_image.h to flip loaded texture's on the y-axis (before loading model).
    stbi_set_flip_vertically_on_load(true);

    // configure global opengl state
    // -----------------------------
    glEnable(GL_DEPTH_TEST);

    // build and compile shaders
    // -------------------------

    vector<Model> crowd;
    int crowd_size = 4;
    Shader spider_shader("../shaders/light.vert", "../shaders/light.frag");
    char modelPath[] = "../models/spider.obj";
    char modelHierarchyPath[] = "../models/spider.json";
    glm::mat4 model = glm::mat4(1.0f);
    auto scaleMatrix = glm::vec3(0.01f, 0.01f, 0.01f);
    model = glm::scale(model, scaleMatrix);
    
    for (int i = 0; i < crowd_size; ++i){
      Model spider_model(modelPath, modelHierarchyPath, true);
      glm::mat4 initial_position = glm::translate(model, glm::vec3(0.0f, 0.0f, i * 400.0f - 600.0f));
      spider_model.hierarchy.setTransform("root", initial_position);
      spider_model.hierarchy.compileTransforms();
      spider_model.hierarchy.resetParents();
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
    } else
      std::cout << "Failed to load texture" << std::endl;
    
    stbi_image_free(data);
    
    unsigned int indices[] = {  
      0,1,2,3,1,2
			      
    };
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

	 
    // render loop
    // -----------
    while (!glfwWindowShouldClose(window))
      {
        // per-frame time logic
        // --------------------
        float currentFrame = glfwGetTime();
        deltaTime = currentFrame - lastFrame;
        lastFrame = currentFrame;

	world_time += deltaTime;
        // input
        // -----
        processInput(window);

        // render
        // ------
        glClearColor(0.05f, 0.05f, 0.05f, 1.0f);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

        // don't forget to enable shader before setting uniforms

        // view/projection transformations
        glm::mat4 projection = glm::perspective(glm::radians(camera.Zoom), (float)SCR_WIDTH / (float)SCR_HEIGHT, 0.1f, 100.0f);
        glm::mat4 view = camera.GetViewMatrix();
	spider_shader.use();
        spider_shader.setMat4("projection", projection);
        spider_shader.setMat4("view", view);

	glm::vec3 view_position = camera.GetViewPosition();
	spider_shader.setVec3("view_position", view_position);
	spider_shader.setVec3("light_position", light_position_world);
	crowd_iteration(crowd, spider_shader);
	

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
        // -------------------------------------------------------------------------------
        glfwSwapBuffers(window);
        glfwPollEvents();
    }

    // glfw: terminate, clearing all previously allocated GLFW resources.
    // ------------------------------------------------------------------
    glfwTerminate();
    return 0;
}

// process all input: query GLFW whether relevant keys are pressed/released this frame and react accordingly
// ---------------------------------------------------------------------------------------------------------
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
// ---------------------------------------------------------------------------------------------
void framebuffer_size_callback(GLFWwindow* window, int width, int height)
{
    // make sure the viewport matches the new window dimensions; note that width and 
    // height will be significantly larger than specified on retina displays.
    glViewport(0, 0, width, height);
}

// glfw: whenever the mouse moves, this callback is called
// -------------------------------------------------------
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

void spiderAnimate(MeshHierarchy &model_hierarchy){
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
    float rotation = i % 2 == 0 ? -0.01 : 0.01;
    spider_model.Rotate(rotation * (1 + i));
    spider_model.MoveForward(4 * ((i+1) * 0.8));
    spiderAnimate(spider_model.hierarchy);
    spider_model.hierarchy.compileTransforms();
    spider_model.hierarchy.resetParents();
    spider_model.Draw(spider_shader);
  }
  
  // for (auto& spider_model : crowd){
  //   spider_model.Rotate(0.01);
  //   spider_model.MoveForward(4);
  //   spiderAnimate(spider_model.hierarchy);
  //   spider_model.hierarchy.compileTransforms();
  //   spider_model.hierarchy.resetParents();
  //   spider_model.Draw(spider_shader);
  // }
}
