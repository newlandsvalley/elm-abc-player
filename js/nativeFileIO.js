myapp.ports.requestLoadFile.subscribe(loadFile);

function loadFile() {
    var selectedFile = document.getElementById('fileinput').files[0];
    // console.log("selected file: " + selectedFile);
    var reader = new FileReader();
    reader.onload = function(event) {
      var contents = event.target.result;
      var filespec = {contents:contents, name:selectedFile.name};
      // console.log("File contents: " + contents);
      // console.log("File name: " + selectedFile.name);
      myapp.ports.fileLoaded.send(filespec);
    };

    reader.onerror = function(event) {
      // console.error("File could not be read! Code " + event.target.error.code);
      myapp.ports.fileLoaded.send(null);
    };

    if (selectedFile == undefined) {
       myapp.ports.fileLoaded.send(null);
    } else {
       reader.readAsText(selectedFile);
    }
}

myapp.ports.requestSaveFile.subscribe(saveFile);

function saveFile(filespec) {
    var a = document.createElement("a");
    // console.log("File contents: " + filespec.contents);
    var file = new Blob([filespec.contents], {type: "text/plain;charset=utf-8"});
    url = URL.createObjectURL(file);
    a.href = url
    a.download = filespec.name;
    document.body.appendChild(a);
    a.click();
    setTimeout(function(){
        document.body.removeChild(a);
        window.URL.revokeObjectURL(url);  
    }, 100);  
    myapp.ports.fileSaved.send(true);
}



