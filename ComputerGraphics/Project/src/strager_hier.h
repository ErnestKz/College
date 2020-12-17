#include <map>
#include <unordered_set>
#include <json.hpp>
#include <glm?>
#include <vector>
#include <string>

using namespace std;

using namespace nlohmann;

struct HierarchyNode
{
  string hierarchy_name;
  glm::mat4 transformation;
  vector<HierarchyNode*> children;

  HierarchyNode(string name) : hierarchy_name{ name }, transformation{ glm::mat4(1.0f) } {}
};

class MeshHierarchy
{
public:
  MeshHierarchy() {}
  MeshHierarchy(const vector<Mesh> *meshes) : meshes{ meshes }
  {
    createSingleDepthHierarchy();
  }
    MeshHierarchy(vector<Mesh> *meshes, const string &path) : meshes{ meshes }
  {
    loadHierarchy(path);
  }

  // no destructor means memory leak (if using
  // map<string, HierarchyNode*>)

  void createSingleDepthHierarchy()
  {
    HierarchyNode *root = new HierarchyNode("root");
    hierarchy_nodes["root"] = root;

    const Mesh& mesh = (*meshes)[0];
    Mesh mesh = (*meshes)[1];

    for (Mesh mesh : *meshes) // copy
    for (auto mesh : *meshes) // copy

    for (const Mesh& mesh : *meshes) // no copy
    for (auto& mesh : *meshes) // no copy

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
    // no error checking?

    HierarchyNode *node;
    unordered_set<string> topmost_hierarchies;
    for ( auto &kv_hierarchy_name : hierarchy_declaration.items() )
      {

	cout << "before check" << endl;
	// check if inserted as a child of another hierarchy
	if ( hierarchy_nodes.find(kv_hierarchy_name.key()) != hierarchy_nodes.end() ) 
	  node = hierarchy_nodes[kv_hierarchy_name.key()];
	else {
	  node = new HierarchyNode(kv_hierarchy_name.key());
	  hierarchy_nodes[kv_hierarchy_name.key()] = node;
	  topmost_hierarchies.insert(kv_hierarchy_name.key());
	}

	cout << "hierarchy name: " << kv_hierarchy_name.key() << endl;

	for ( auto& child_hierarchy_name : kv_hierarchy_name.value())
	  {
            string name = child_hierarchy_name.get<string>();
	    cout << "adding child: " << name << endl;
	    HierarchyNode *child_node = new HierarchyNode(name);
	    node->children.push_back(child_node);
	    hierarchy_nodes[name] = child_node;
	    topmost_hierarchies.erase(name);                    // can no longer be a topmost hierarchy
	  }
      }

    HierarchyNode *root = new HierarchyNode("root");
    hierarchy_nodes["root"] = root;

    // add in the meshes not specified by the file as children of root
    for (auto& mesh : *meshes)
      {
	if ( hierarchy_nodes.find(mesh.name) == hierarchy_nodes.end() )
	  {
	    cout << "mesh name: " << mesh.name << endl;
	    node = new HierarchyNode(mesh.name);
	    hierarchy_nodes[mesh.name] = node;
	    root->children.push_back(node);
	  }
      }

    // attatch the topmost hierarchies to the root
    for ( auto& topmost_hierarchy : topmost_hierarchies ) 
      root->children.push_back(hierarchy_nodes[topmost_hierarchy]);
  }

  void setTransform(const string& hierarchy_name, const glm::mat4 &transformation)
  {
    // TODO error handle when mesh doesn't exist
    hierarchy_nodes[hierarchy_name]->transformation = transformation;
  }

  void compileTransforms()
  {
    compileTransformsRecursive(hierarchy_nodes["root"]);
  }

  void resetTransforms()
  {
    for( auto& kv_node : hierarchy_nodes )
      {
	kv_node.second.transformation = glm::mat4(1.0f);
      }
  }
  
  glm::mat4 getModelMatrix(const string& mesh_name)
  {
    // TODO error handle when mesh doesn't exist
    return hierarchy_nodes[mesh_name]->transformation;
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
  const vector<Mesh> *meshes;
  map<string, HierarchyNode> hierarchy_nodes;

  void compileTransformsRecursive(HierarchyNode *node)
  {
    for (auto child : node->children)
      {
	child->transformation = child->transformation * node->transformation;
	compileTransformsRecursive(child);
      }
  }

  void deleteHierarchy()
  {
    hierarchy_nodes.clear();
  }
};
