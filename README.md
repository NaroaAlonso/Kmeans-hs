# Kmeans-hs
An implementation of KMeans algorithm in haskell, with a kNN-1 Classifier

KMeans is a machine learning algorithm used to classify a training set into an n number of Clusters. It belongs to the group of non-supervised learning algorithms so it does not decide whether or not the chosen number of clusters is appropriate or which classifications belongs to each one. It is implemented in pure functional language Haskell.
This implementation is designed to process CSV data files.
Note: the only distance used in order to calculate the reach is the euclidean one. In order to use a different one, the code must be added in the KMeans.hs module.

## Dependencies
GHCI needed in order to use Haskell.
Haskell packages used in the implementation:

    • Text.CSV  
    Documentation at: http://hackage.haskell.org/package/csv-0.1.2/docs/Text-CSV.html
  
    • System.Environment  
    Documentation at: http://hackage.haskell.org/package/base-4.12.0.0/docs/System-Environment.html
  
    • Data.Ord  
    Documentation at: http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Ord.html
  
    • Data.List  
    Documentation at: http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html
  

## Description
The package includes a module called KMeans.hs and the main interactive one, called Main.hs.
KMeans.hs module includes three visible functions: kmedias (to obtain centroids using kmeans), classifyUno (classifies one only value) , classsifyLista (classifies a list of values). They all receive the training set, the number of clusters and the number of attributes, and classifyUno receives a value and classifyLista receives a list of values.
In order to train the set, we need the function algorithm, which calls the other functions like this.
What algorithm does is:


 • Function **centrosInic**. It takes the first n points from the training set, being n the number of clusters. These are going to be the first centroids. 


 • Function **asignaCluster**. Assigns each point of the training set to a cluster, represented by an Integer. It returns a list of the pairs (point, cluster).


 • Function **procesaCentros**. It returns a list of lists. These sublists contain every point belonging to each cluster.


 • Function **calculaCentro**. Calculates the average of every point belonging to each cluster, thus returning the new list of centroids.


 • Function **nuevosCentros**. It runs the data through procesaCentros and calculaCentro, so it is all condensed in the same function.



## Usage
This particular implementation becomes easy to use even for those who do not have experience using Haskell, since it runs entirely through an I/O interface.
It is only necessary to start ghci on a terminal in the directory, :l Main.hs and write main in the terminal. Then, the packages will be loaded.
First step: indicating the path to the file going to be used as a training set. If no path is indicated, the default one will be used: data/iris.csv.
Second step: indicating the number of attributes used for the classification, and then the number of clusters wanted. Bear in mind that the number of clusters must be entirely chosen by the person using the algorithm. Maybe it is needed to try with a different number each time, until the required results are obtained.
After that, the settings are complete. We just need to choose what we need:

  1. To visualize the centroids, press 1

Note: there is a possibility to export the results to a file. Just press 1 and write down the name of the file where the results are wanted.


  2. To classify a single value, press 2 and introduce the value in a double list format.


  3. To classify several values stored in a CSV File, press 3 and introduce the path of the file. Afterwards, press 1 for exporting it into a file and write down the path.


The results shown in options 2 and 3 in the menu are:

POINT TO CLASSIFY

ASSIGNED CLUSTER

CLUSTER CENTROID

### Contributing
Please feel welcome to suggest any changes or improvements.

### Author
Naroa Alonso Fernández. Seville, 2019.
