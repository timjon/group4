﻿# [FUML: Making UML Fun]
 
 <img width="200" height="200" src="https://github.com/timjon/group4/blob/develop/Application/src/resources/logo.png">
 

# Authors 🖋️

Tim Jonasson, Pontus Laestadius, Rashad Kamsheh, Sebastian Fransson, Kosara Golemshinska and Isabelle Törnqvist.
 
# Third party libraries

- [gson library](https://github.com/google/gson)
is a Java serialization/deserialization library to convert Java Objects into JSON and back

To use Gson for for Deserialization (parsing JSON into Java Objects), make sure that you have the the gson-2.8.2.jar file which you can find in /Application/lib/

Gson basics:

let's say you have the following JSON format:

```
{
  "name": "shashi",
  "email": "shashi@fuml.io",
  "age": 21,
  "isStudent": true
}
```
To parse into Java Objects, create the corresponding Java boilerplate Class:

```
public class Author {  
    String name;
    String email;
    int age;
    boolean isStudent;
}
```

The next step is creating a Gson instance:
```
Gson gson = new Gson();  
```
And the final step is mapping from a JSON to a Java object using fromJson():
```
Author authorObject = gson.fromJson(yourJsonString, Author.class);  
```
And you are all set!
