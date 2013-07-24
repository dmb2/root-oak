#include "TTabCom.h"
#include <iostream>
#include "TCollection.h"
#include "TSeqCollection.h"

void tabComTest(){
  TTabCom tabCom;
  const TSeqCollection* classes=tabCom.GetListOfGlobals();
  for(int i=0; i < classes->GetSize(); i++){
    std::cout<<classes->At(i)->GetName()<<std::endl;
  }
  std::cout<<"number of Cpp directives: "<<tabCom.GetListOfCppDirectives()->GetSize()<<endl;
  std::cout<<"number of classes: "<<tabCom.GetListOfClasses()->GetSize()<<endl;
  std::cout<<"number of Globals: "<<tabCom.GetListOfGlobals()->GetSize()<<endl;
  std::cout<<"number of Global Functions: "<<tabCom.GetListOfGlobalFunctions()->GetSize()<<endl;
  std::cout<<"number of Pragmas: "<<tabCom.GetListOfPragmas()->GetSize()<<endl;
}
