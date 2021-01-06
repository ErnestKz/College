#include <glad/glad.h>
#include <GLFW/glfw3.h>

#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>

#define STB_IMAGE_IMPLEMENTATION
#include <stb_image.h>

#include <iostream>
#include <math.h>

int main() {
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
      draw_crowd(crowd, spider_shader);
      
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
