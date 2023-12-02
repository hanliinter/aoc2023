#include <iostream>
#include <fstream>
//#include <sstream>>
#include <vector>
#include <string>




int solve_part1(std::vector<std::string> &content){
    std::vector<int> result;
    int sum = 0;
    for (auto v : content){
      size_t ia = v.find_first_of("123456789");
      size_t ib = v.find_last_of("123456789");
      sum += (v[ia] - '0') * 10 + (v[ib] - '0');
    }
    return sum;

}

std::string &  replace(std::string &niddle, std::string &newNiddle, std::string &hay){
  while (hay.find(niddle) != std::string::npos){
    int p = hay.find(niddle);
    hay.replace(p,niddle.length(),newNiddle);
  }
  return hay;
}
  

int solve_part2(std::vector<std::string> &content){
  std::string names[9] = {"one",
         "two",
         "three",
         "four",
         "five",
         "six",
         "seven",
          "eight",
          "nine"};


    std::string replacementNames[9] = {"o1ne",
         "t2wo",
         "t3hree",
         "f4our",
         "f5ive",
         "s6ix",
         "s7even",
          "e8ight",
          "n9ine"};

    std::vector<std::string> result;
    for (auto v : content){
      for (int i= 0; i< 9; i++){
	replace(names[i], replacementNames[i], v);
      }
      result.push_back(v);
    }
    return solve_part1(result);
 
}

int main(int argc, char *argv[])
{
  std::ifstream infile("input.txt");
  std::string line;
  std::vector<std::string> content;
  while (std::getline(infile,line)){
    content.push_back(line);
  }

  // for (auto v: content){
  //   std::cout<<v<<"\n";
  // }
  // solve_part1(content);
  std::cout<<solve_part2(content);
  return 0;
}

