#include <map>
#include <unordered_set>
#include <json.hpp>

using namespace nlohmann;

struct HierarchyNode
{
  string hierarchy_name;
  glm::mat4 transformation;
  glm::mat4 rotation;
  vector<HierarchyNode*> children;

  HierarchyNode(string name) : hierarchy_name{ name },
			       transformation{ glm::mat4(1.0f) },
			       rotation{ glm::mat4(1.0f) } {}
  ~HierarchyNode() {}
};

class MeshHierarchy
{
public:
  MeshHierarchy() {}
  MeshHierarchy(vector<Mesh> *meshes) : meshes{ meshes }
  {
    createSingleDepthHierarchy();
  }
    MeshHierarchy(vector<Mesh> *meshes, const string &path) : meshes{ meshes }
  {
    loadHierarchy(path);
  }
  ~MeshHierarchy() {}

  void createSingleDepthHierarchy()
  {
    HierarchyNode *root = new HierarchyNode("root");
    hierarchy_nodes["root"] = root;
    for (auto mesh : *meshes)
      {
	HierarchyNode *node = new HierarchyNode(mesh.name);
	hierarchy_nodes[mesh.name] = node;
	root->children.push_back(node);
      }
  }
  
  void loadHierarchy(const string &path)
  {
    deleteHierarchy();
    
    ifstream hierarchy_file(path);
    json hierarchy_declaration;
    hierarchy_file >> hierarchy_declaration;
    HierarchyNode *node;
    unordered_set<string> topmost_hierarchies;
    for ( auto &kv_hierarchy_name : hierarchy_declaration.items() )
      {
	// check if inserted as a child of another hierarchy
	if ( hierarchy_nodes.find(kv_hierarchy_name.key()) != hierarchy_nodes.end() ) 
	  node = hierarchy_nodes[kv_hierarchy_name.key()];
	else {
	  node = new HierarchyNode(kv_hierarchy_name.key());
	  hierarchy_nodes[kv_hierarchy_name.key()] = node;
	  topmost_hierarchies.insert(kv_hierarchy_name.key());
	}



	for ( auto child_hierarchy_name : kv_hierarchy_name.value())
	  {
	    HierarchyNode *child_node = new HierarchyNode(child_hierarchy_name.get<string>());
	    node->children.push_back(child_node);
	    hierarchy_nodes[child_hierarchy_name.get<string>()] = child_node;
	    topmost_hierarchies.erase(child_hierarchy_name.get<string>());                    // can no longer be a topmost hierarchy
	  }
      }

    HierarchyNode *root = new HierarchyNode("root");
    hierarchy_nodes["root"] = root;

    // add in the meshes not specified by the file as children of root
    for (auto mesh : *meshes)
      {
	if ( hierarchy_nodes.find(mesh.name) == hierarchy_nodes.end() )
	  {
	    node = new HierarchyNode(mesh.name);
	    hierarchy_nodes[mesh.name] = node;
	    root->children.push_back(node);
	  }
      }

    // attatch the topmost hierarchies to the root
    for ( auto topmost_hierarchy : topmost_hierarchies ) 
      root->children.push_back(hierarchy_nodes[topmost_hierarchy]);
  }

  void setTransform(string hierarchy_name, glm::mat4 &transformation)
  {
    hierarchy_nodes[hierarchy_name]->transformation = transformation;
  }

  void setRotation(string hierarchy_name, glm::mat4 &transformation)
  {
    hierarchy_nodes[hierarchy_name]->rotation = transformation;
  }
  
  void addTransform(string hierarchy_name, glm::mat4 &transformation)
  {
    hierarchy_nodes[hierarchy_name]->transformation = transformation * hierarchy_nodes[hierarchy_name]->transformation;
  }
  void addRotation(string hierarchy_name, glm::mat4 &transformation)
  {
    hierarchy_nodes[hierarchy_name]->rotation = transformation * hierarchy_nodes[hierarchy_name]->rotation;
  }

  
  void compileTransforms()
  {
    compileTransformsRecursive(hierarchy_nodes["root"]);
    compileRotationsRecursive(hierarchy_nodes["root"]);
  }

  void resetTransforms()
  {
    for( auto kv_node : hierarchy_nodes )
      {
	kv_node.second->transformation = glm::mat4(1.0f);
	kv_node.second->rotation = glm::mat4(1.0f);
      }
  }

  void resetParents()
  {
     for( auto kv_node : hierarchy_nodes )
      {
	if (kv_node.second->children.size() > 0){
	  kv_node.second->transformation = glm::mat4(1.0f);
	  kv_node.second->rotation = glm::mat4(1.0f);
	}
      }
  }
  
  glm::mat4 getModelMatrix(string mesh_name)
  {
    // TODO error handle when mesh doesn't exist
    return hierarchy_nodes[mesh_name]->transformation * hierarchy_nodes[mesh_name]->rotation ;
  }

  void printHierarchy(const string& name){
    cout << name << endl;
    for (auto child : hierarchy_nodes[name]->children)
      {
	printHierarchy(child);
      }
  }

  void printHierarchy(HierarchyNode *node){
    cout << node->hierarchy_name << endl;
    for (auto child : node->children)
      {
	printHierarchy(child);
      }
  }

private:
  vector<Mesh> *meshes;
  map<string, HierarchyNode*> hierarchy_nodes;

  void compileTransformsRecursive(HierarchyNode *node)
  {
    for (auto child : node->children)
      {
	child->transformation = child->transformation * node->transformation;
	compileTransformsRecursive(child);
      }
  }

    void compileRotationsRecursive(HierarchyNode *node)
  {
    for (auto child : node->children)
      {
	child->rotation = child->rotation * node->rotation;
	compileRotationsRecursive(child);
      }
  }

  void deleteHierarchy()
  {
    for ( auto kv_node : hierarchy_nodes )
      delete kv_node.second;
    
    hierarchy_nodes.clear();
  }
};
// put code in namespace
// don't miss includes
// delete empty destructor

// documentation on star pionter
// put const



// const auto & mesh
