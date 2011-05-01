package gw.storage.javasvn;

import gw.storage.StorageException;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.List;

import org.tmatesoft.svn.core.SVNCommitInfo;
import org.tmatesoft.svn.core.SVNException;
import org.tmatesoft.svn.core.SVNNodeKind;
import org.tmatesoft.svn.core.io.ISVNEditor;
import org.tmatesoft.svn.core.io.ISVNReporter;
import org.tmatesoft.svn.core.io.diff.SVNDiffWindow;
import org.tmatesoft.svn.core.io.diff.SVNDiffWindowApplyBaton;
import org.tmatesoft.svn.core.wc.ISVNMerger;
import org.tmatesoft.svn.core.wc.ISVNMergerFactory;
import org.tmatesoft.svn.core.wc.SVNStatusType;
import org.tmatesoft.svn.core.wc.SVNWCUtil;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * This class can be used to represent a collection of paths as a tree. For example,
 * if you pass a Collection with the following entries to the constructor:
 * <br /><br />
 * <verbatim>
 * dir1 (type SVNNodeKind.DIR)
 * dir1/file1 (type SVNNodeKind.FILE)
 * dir2 (type SVNNodeKind.DIR)
 * dir2/dir22/file5 (type SVNNodeKind.FILE)
 * </verbatim>
 * <br /><br />
 * you would get a tree with dir1 & dir2 on the first level, file1 as a child of dir1,
 * dir22 as a child of dir2 and file5 as a child of dir22. The tree's root will be
 * identified by the empty string and will, in this case, be the parent of dir1 & dir2.
 * <br /><br />
 * 
 * Optionally, you can associate a value with each node. If you don't need to use this,
 * just use null as the value wherever it's requested.
 * <br /><br />
 * All the methods of this class strip all paths that you supply of any leading or
 * trailing slashes.
 *
 * @author Bogdan Dumitriu
 */
public class PathTree<V> implements ISVNEditor
{
    private PathTreeNode<V> _root;
    private String _basePath;
    private V _rootValue;
    private long _revision;
    private boolean _fixed;
    
    /**
     * Builds a new PathTree from a Collection which contains {@link PathTreeNodeData}
     * elements. You should make sure that the path and the type attributes of the
     * elements of the collection are set correctly. Values can be set or left null,
     * depending on whether you need to use them or not. You should <b>not</b> include
     * an entry for the root path (which is the value of <code>basePath</code>) in the
     * <code>entries</code> collection.
     *
     * @param entries a collection of future tree nodes (a null <code>entries</code>
     *                collection is considered empty)
     * @param basePath a (possibly empty) base path which should (logically) be the
     *                 directory which contains the top level entries of the tree. This
     *                 path is relevant only for the {@link #runItem(CommitItem)} method.
     *                 Any trailing or leading slash is removed from <code>basePath</code>
     * @param rootValue if you wish to associate a value with the root of this tree,
     *                  you can specify it here. If not, just use null. See
     *                  {@link #getRootValue()} and {@link #setRootValue(V)} for more
     *                  information on this parameter
     * @param revision the revision number to which <code>entries</code> belong.
     * @throws NodeIsFileException if the collection includes a path which would have
     *                             to be a child of a node representing a file. For
     *                             example, you could have an entry with path "path1"
     *                             with type file and another one with path
     *                             "path1/path2" (type is irrelevant). Then, you'd get
     *                             such an exception
     * @throws NodeExistsException if you have duplicate paths in the collection, the
     *                             constructor will end up trying to insert the same
     *                             path twice, which will result in this exception
     *                             being thrown
     */
    public PathTree(Collection<PathTreeNodeData<V>> entries, String basePath, long revision)
        throws NodeIsFileException, NodeExistsException
    {
        _root = null;
        _basePath = Path.removeSlashes(basePath);
        _rootValue = null;
        _revision = revision;
        _fixed = false;
        _tempFiles = new LinkedList<File>();

        if (entries != null)
        {
            PathTreeNodeData<V>[] sortedEntries = entries.toArray(new PathTreeNodeData[0]);
            Arrays.sort(sortedEntries);
            for (PathTreeNodeData<V> entry : sortedEntries)
            {
                insertNode(entry);
            }
        }
    }

//    public String getBasePath()
//    {
//        return _basePath;
//    }
//
//    /**
//     * Sets the base path to <code>basePath</code>. Any trailing or leading
//     * slash is removed from <code>basePath</code>.
//     */
//    public void setBasePath(String basePath)
//    {
//        _basePath = Util.removeSlashes(basePath);
//    }

    /**
     * Returns the default root value, i.e. the value associated with the root node
     * when it is built automatically. If you have changed the value of the root node
     * using {@link #setNodeValue(String, V)}, you will not get that value back by
     * calling this method (unless the root node has been rebuilt in the meantime).
     * So, if you need that value, use {@link #getNodeValue(String)} instead, specifying
     * the empty string as the path. 
     */
    private V getRootValue()
    {
        return _rootValue;
    }

    /**
     * Sets the default root value. This will not immediately change the value associated
     * with the current root node. If that is what you want, use {@link #setNodeValue(String, V)}
     * with the empty string as the path. Also, see {@link #getRootValue()} for more information.
     */
    private void setRootValue(V rootValue)
    {
        _rootValue = rootValue;
    }

    /*public void setRevision(long revision)
    {
        _revision = revision;
    }*/

    public long getRevision()
    {
        return _revision;
    }

    /**
     * Returns true if the current revision number is fixed, i.e., if there are local
     * changes which have to be commited. Whether a working copy is fixed or not is
     * managed automatically by the working copy, so you will find no setter method
     * for this.
     */
    public boolean isFixed()
    {
        return _fixed;
    }

    /**
     * Returns the data associated with the node identified by <code>path</code> or
     * null if no such node exists.
     *
     * @param path the path to the node whose data you want
     * @throws NodeIsFileException if some proper subpath of <code>path</code>
     *                             indicates a file
     */
//    public PathTreeNodeData<V> getNodeData(String path)
//        throws NodeIsFileException
//    {
//        PathTreeNode<V> node = getNode(path);
//        return (node == null) ? null :        
//            new PathTreeNodeData<V>(node.getPath(), node.getType(), node.getValue());
//    }

    /**
     * Returns the type of the node identified by <code>path</code> or SVNNodeKind.NONE
     * if no such node exists.
     *
     * @param path the path to the node whose type you want
     * @throws NodeIsFileException if some proper subpath of <code>path</code>
     *                             indicates a file
     */
//    public SVNNodeKind getNodeType(String path)
//        throws NodeIsFileException
//    {
//        PathTreeNode<V> node = getNode(path);
//        return (node == null) ? null : node.getType();
//        
//    }


    /**
     * Returns the value associated with the node identified by <code>path</code>
     * or null if no such node exists or if the value associated with it is null.
     *
     * @param path the path to the node whose value you want
     * @throws NodeIsFileException if some proper subpath of <code>path</code>
     *                             indicates a file
     */
//    public V getNodeValue(String path)
//        throws NodeIsFileException
//    {
//        PathTreeNode<V> node = getNode(path);
//        return (node == null) ? null : node.getValue();
//        
//    }

    /**
     * Sets the value of the node identified by <code>path</code> to <code>value</code>.
     *
     * @param path the path of the node whose value you want to set
     * @param value the value you want to set
     * @return true, if value was set successfully or false if node was not found
     * @throws NodeIsFileException if some proper subpath of <code>path</code>
     *                             indicates a file
     */
//    public boolean setNodeValue(String path, V value)
//        throws NodeIsFileException
//    {
//        PathTreeNode<V> node = getNode(path);
//        if (node == null)
//        {
//            return false;
//        }
//        else
//        {
//            node.setValue(value);
//            return true;
//        }
//    }

    /**
     * Returns the commit item associated with the node identified by <code>path</code>
     * or null if no such node exists or if there is no commit item associated with the
     * node.
     *
     * @param path the path to the node whose commit item you want
     * @throws NodeIsFileException if some proper subpath of <code>path</code>
     *                             indicates a file
     */
    public CommitItem getNodeChanges(String path)
        throws NodeIsFileException
    {
        PathTreeNode<V> node = getNode(path);
        return (node == null) ? null : node.getChanges();
        
    }

    /**
     * Sets the commit item of the node identified by <code>path</code> to <code>changes</code>.
     *
     * @param path the path of the node whose commit item you want to set
     * @param changes the commit item you want to set
     * @return true, if commit item was set successfully or false if node was not found
     * @throws NodeIsFileException if some proper subpath of <code>path</code>
     *                             indicates a file
     */
    public boolean setNodeChanges(String path, CommitItem changes)
        throws NodeIsFileException
    {
        PathTreeNode<V> node = getNode(path);
        if (node == null)
        {
            return false;
        }
        else
        {
            node.setChanges(changes);
            return true;
        }
    }

    /**
     * Returns the base file associated with the node identified by <code>path</code>
     * or null if no such node exists or if there is no base file associated with the
     * node.
     *
     * @param path the path to the node whose base file you want
     * @throws NodeIsFileException if some proper subpath of <code>path</code>
     *                             indicates a file
     */
    public File getNodeBaseFile(String path)
        throws NodeIsFileException
    {
    	    PathTreeNode<V> node = getNode(path);
        return (node == null) ? null : node.getBaseFile();
         
    }

    /**
     * Returns the conflict state associated with the node identified by <code>path</code>
     * or false if no such node exists.
     *
     * @param path the path to the node whose conflict state you want
     * @throws NodeIsFileException if some proper subpath of <code>path</code>
     *                             indicates a file
     */
    public boolean getNodeConflictState(String path)
        throws NodeIsFileException
    {
        PathTreeNode<V> node = getNode(path);
        return (node == null) ? false : node.isInConflict();
        
    }

    /**
     * Sets the conflict state of the node identified by <code>path</code> to
     * <code>conflictState</code>.
     *
     * @param path the path of the node whose conflict state you want to set
     * @param conflictState the conflict state you want to set
     * @return true, if conflict state was set successfully or false if node was not found
     * @throws NodeIsFileException if some proper subpath of <code>path</code>
     *                             indicates a file
     */
    public boolean setNodeConflictState(String path, boolean conflictState)
        throws NodeIsFileException
    {
        PathTreeNode<V> node = getNode(path);
        if (node == null)
        {
            return false;
        }
        else
        {
            node.setInConflict(conflictState);
            return true;
        }
    }

    /**
     * Returns the value associated with the parent of the node identified by
     * <code>path</code> or null if either the node doesn't exist or if its parent
     * is null (only for root node) or if its parent's value is null.
     *
     * @param path the path of the node whose parent's value you want
     * @throws NodeIsFileException if some proper subpath of <code>path</code>
     *                             indicates a file
     */
//    public V getParentValue(String path)
//        throws NodeIsFileException
//    {
//        PathTreeNode<V> node = getNode(path);
//        if (node != null )
//        {
//            node = node.getParent();
//            return (node == null) ? null : node.getValue();
//        }
//        else
//        {
//            return null;
//        }
//    }

    /**
     * Returns true if a node identified by <code>path</code> exists in the tree
     * and false otherwise.
     *
     * @param path the path of the node whose existence you want to check
     */
    public boolean hasNode(String path)
    {
        boolean result;

        try
        {
            result = getNode(path) != null;
        }
        catch (NodeIsFileException e)
        {
            return false;
        }

        return result;
    }

    /**
     * Returns true if a node identified by <code>path</code> exists in the tree
     * and is of type file and false otherwise.
     *
     * @param path the path of the node whose existence you want to check
     */
    public boolean hasFileNode(String path)
    {
        boolean result;
        try
        {
            PathTreeNode<V> node = getNode(path);
            result = node != null && node.getType() == SVNNodeKind.FILE;
        }
        catch (NodeIsFileException e)
        {
            return false;
        }

        return result;
    }

    /**
     * Returns true if a node identified by <code>path</code> exists in the tree
     * and is of type directory and false otherwise.
     *
     * @param path the path of the node whose existence you want to check
     */
    public boolean hasDirNode(String path)
    {
        boolean result;
        try
        {
            PathTreeNode<V> node = getNode(path);
            result = node != null && node.getType() == SVNNodeKind.DIR;
        }
        catch (NodeIsFileException e)
        {
            return false;
        }

        return result;
    }

    /**
     * Returns a Map with all the paths and corresponding values of the children or descendents
     * of the node identified by <code>path</code>.
     *
     * @param path the path whose children's (and possibly descendents') values you want
     * @param includeRoot set to true if you want the node corresponding to <code>path</code>
     *                    to be included in the result, together with its associated value
     * @param recurse set to true if you want the values of all the descendents of the
     *                node identified by <code>path</code>. If set to false, then only the
     *                direct children of this node are returned.
     * @return a Map of paths with their corresponding values.
     */
    public Map<String, V> getNodeValues(String path, boolean includeRoot, boolean recurse)
    {
        PathTreeNode<V> node;
        try
        {
            node = getNode(path);
        }
        catch (NodeIsFileException e)
        {
            node = null;
        }
        
        Map<String, V> result;
        if (node == null )
        {
            result = new HashMap<String, V>();
        }
        else
        {
            result = getNodeValues(node, recurse);
            if (includeRoot)
            {
                result.put("/" + node.getPath(), node.getValue());
            }
        }

        return result;
    }

    /**
     * Returns a collection of all the paths which have their conflict state set.
     */
    public Collection<String> getConflicts()
    {
        return getConflicts(_root);
    }

    /**
     * Returns a collection of all the commit items in the tree. The commit items are
     * added to the list by doing a preorder traversal of the tree.
     */
    public Collection<CommitItem> getCommitItems()
    {
        return getCommitItems(_root);
    }

    /**
     * TODO: Comment this, please.
     */
    public String getRealPath(String path)
        throws NodeIsFileException
    {
        path = Path.removeSlashes(path);
        
        String[] pathElems = Path.removeSlashes(path).split("/");
        String fileName = pathElems[pathElems.length -1];
        PathTreeNode<V> node = null;
        
        try
        {
            node = getNode(path);
        }
        catch(Exception e)
        {
            throw new NodeIsFileException(e.toString());
        }
        
        if(node != null)
        {
            CommitItem changes = node.getChanges();
            if(changes != null)
            {
                String copyPath = changes.getCopyFromPath();
                
                if(copyPath != null)
                {
                    return copyPath;
                }
                
            }
            
            PathTreeNode<V> parent = node.getParent();
            if((parent != null) && (parent != _root))
            {
                
                String base = path.substring(0, path.lastIndexOf(fileName));
                return (getRealPath(base) + "/" + fileName);
            }
            
        }
        
        return fileName;
    }

    /**
     * Registers the creation of a new directory.
     *
     * @path path the path to the directory you want to create
     * @throws NodeIsFileException if some proper subpath of <code>path</code>
     *                             indicates a file
     * @throws NodeExistsException if a node with the same path already exists in the
     *                             tree
     */
    public void registerMkdir(String path)
        throws NodeIsFileException, NodeExistsException
    {
        path = Path.removeSlashes(path);

        PathTreeNode<V> node;
        node = insertNode(new PathTreeNodeData<V>(path, SVNNodeKind.DIR, null), true);

        assert node != null;    // if node was successfully inserted, then it should
                                // also be possible to successfully retrieve it

        node.setChanges(new CommitItem(true, path, true));
        node.getChanges().setBaseRevision(_revision);

        _fixed = true;
    }

    /**
     * Registers either the creation of a new file with the given <code>contents</code>
     * or the changing of the contents of an already existing file. <code>contents</code>
     * can be either null or an empty file if you want to register the creation of an
     * empty file.
     *
     * @path path the path to the file you want to store
     * @path contents the contents of the file you want to store
     * @path baseContents if you are registering the modification of a file, then you also
     *				      need to pass the base version of that file at the repository version
     *				      which is set as the revision of this path tree
     * @throws NodeIsFileException if some proper subpath of <code>path</code>
     *                             indicates a file
     * @throws NodeIsDirException if <code>path</code> indicates an node in the tree
     *                            which is a directory
     */
    public void registerStoreFile(String path, File contents, File baseContents)
        throws NodeIsFileException, NodeIsDirException
    {
        path = Path.removeSlashes(path);

        PathTreeNode<V> node = getNode(path);
        if (node != null)   // then this is a modify file
        {
            if (node.getType() == SVNNodeKind.DIR)
            {
                throw new NodeIsDirException("Path '" + path + "' is of type DIR.");
            }
            else if (node.getChanges() == null)
            {
                node.setChanges(new CommitItem(false, path, contents, false));
                node.setBaseFile(baseContents);
            }
            else
            {
                node.getChanges().setFile(contents);
                node.setBaseFile(baseContents);
            }
        }
        else    // then this is an add file
        {
            try
            {
                node = insertNode(new PathTreeNodeData<V>(path, SVNNodeKind.FILE, null), true);
                node.setChanges(new CommitItem(false, path, contents, true));
            }
            catch (NodeExistsException e)
            {
                // this cannot be thrown since we have already implicitly checked it
                e.printStackTrace();
            }
        }
        node.getChanges().setBaseRevision(_revision);

        _fixed = true;
    }

    /**
     * Registers the deletion of a file or directory.
     * <br /><br />
     * 
     * If you register a directory for deletion this will recurse over
     * all children of the node and register those for deletion as well.
     * this is needed for revert.
     *
     * @throws NodeIsFileException if some proper subpath of <code>path</code>
     *                             indicates a file
     * @throws NoSuchNodeException if <code>path</code> doesn't point to a valid node
     */
    public void registerDeletePath(String path, boolean force)
        throws NodeIsFileException, NoSuchNodeException
    {
        path = Path.removeSlashes(path);

        PathTreeNode<V> node = getNode(path);
        if (node != null)
        {
            CommitItem changes = node.getChanges();
            if (changes == null)    // this is a node that exists in the repository,
                                    // so delete all its children and mark if for
                                    // deletion
            {
                if (node.getType() == SVNNodeKind.DIR)
                {
                    //Children will not be removed from the node but delete commititems for each node will be created
                    //node.deleteAllChildren();
                    
                    //Recursively create delete commititems for all children of this node
                    registerDeleteChildren(node, force);
                    
                    node.setChanges(new CommitItem(true, path));
                }
                else
                {
                    node.setChanges(new CommitItem(false, path));
                }
                node.getChanges().markAsDelete();
            }
            else
            {
                if (!force)
                {
                    return;
                }

                // this means that the path exists in the repository, we first deleted
                // it locally, then we readded locally, and now we redelete it locally
                // the composed effect is that we simply delete it locally
                if (changes.isAdd() && changes.isDelete())
                {
                    if (node.getType() == SVNNodeKind.DIR)
                    {
                        //Children will not be removed from the node but delete commititems for each node will be created
                        //node.deleteAllChildren();
                        
                        //Recursively create delete commititems for all children of this node
                        registerDeleteChildren(node, force); 
                    }
                    node.getChanges().unmarkAsAdd();
                    node.getChanges().unmarkAsCopy();
                }
                // this means that the path does not exist in the repository, we added
                // it locally, so deleting it locally means simply deleting the node
                // altogether, since nothing has to be committed to the repository
                // NOTE: this also holds for copied (and moved) files or directories
                else if (changes.isAdd())
                {
                    String[] pathElems = path.split("/");
                    String name = pathElems[pathElems.length - 1];
                    node.getParent().deleteChild(name);
                }
            }
            node.getChanges().setBaseRevision(_revision);
        }
        else
        {
            throw new NoSuchNodeException("Path '" + path + "' does not indicate a valid node.");
        }

        _fixed = true;
    }
    
    /**
     * revertFile reverts a single file to it currunt status in the repo
     * NOTE that we provide slightly different implementation/functionality then the
     * svn commandline client. If you revert a file in a deleted directory
     * we will also revert the direrctory, all though all other files in that 
     * directory will stay deleted.
     * @param path the path of the file to revert
     */    
    public void revertFile(String path)
        throws NodeIsFileException, NoSuchNodeException, NodeExistsException
    {
        //System.out.print(path);
        path = Path.removeSlashes(path);  
        String[] pathElems = path.split("/");
        int length = (pathElems.length);
        
        /*
         * Works like the svn commandline client does
         * revert the single file pointed to
         */
        PathTreeNode<V> node = getNode(path,false);
        if(node != null)
        {
            CommitItem changes = node.getChanges();
            if(changes != null)
            {
                PathTreeNode<V> currentParent = node.getParent();
                if(changes.isMove())
                {
                    //when reverting a move we will have to reinstate it in its original path....
                    //hoping that there have been no changes in the original place.....
                    //if so for now i throw an error that the move is not going to complete
                    //fidly stuff here
                    String oldPath = changes.getMoveFromPath();
                    
                    try
                    {
                        insertNode(oldPath, node.getType(), null);
                    }
                    catch(Exception e)
                    {
                        throw new NodeExistsException("The original path of this move you are trying to revert has changes please delete or revert those first");
                    }
                    
                    PathTreeNode<V> oldNode = getNode(oldPath);
                    PathTreeNode<V> oldParent = oldNode.getParent();
                    
                    oldParent.deleteChild(pathElems[length -1]);
                    oldParent.addChild(pathElems[length -1],node);
                    node.setParent(oldParent);
                    rebuildTree(node, oldPath);
                    if(!changes.isCopy())
                    {
                        //this means it was a move of a locally added file
                        changes.unmarkAsMove();
                        changes.markAsAdd();
                    }
                    else
                    {
                        changes.unmarkAsMove();
                    }
                        
                    rebuildTree(node,oldPath);
                    
                    currentParent.deleteChild(pathElems[length -1]);
                    
                }
                else if(changes.isAdd())
                {
                    //If the file is new remove it from the parent for consistency
                    //String name = pathElems[length -1];
                    currentParent.deleteChild(pathElems[length -1]);
                    
                }
                else
                {
                    node.setChanges(null);
                }
            }
        }
        else
        {
            throw new NoSuchNodeException("Path '" + path + "' does not indicate a valid node.");
        }
        
        
        
        /*
         * now check if none of the dir's in the path is set for deletion
         * if they are then remove the delete flag
         */
        
        
        
        //System.out.println(length);
        String tempPath = "";
        for(int i = 0; i < (length - 1); i++)
        {
            tempPath += ("/" + pathElems[i]);
            PathTreeNode<V> tmpNode = getNode(tempPath,false);
            
            
            // we can asume all nodes are there since otherwise we wouldn't have
            // come this far
            if(tmpNode.isLogicallyDeleted())
            {
                //the changes must exist otherwise isLogicallyDeleted would fail
                
                CommitItem changes = tmpNode.getChanges();
                changes.unmarkAsDelete();
            }
            
        }       
        
    }
    
    /**
     * Calls registerDelete for all children of the given node
     * 
     * @param node the node of which all children will be registered dor deletion
     * @param force passed on to registerDelete (unused for now)
     * @throws NodeIsFileException
     * @throws NoSuchNodeException
     */ 
    //This is a rather dirty hack but suffices for now
    private void registerDeleteChildren(PathTreeNode<V> node, boolean force)
        throws NodeIsFileException, NoSuchNodeException
    {
        
        if(node.getType() != SVNNodeKind.DIR)
        {
            throw new NodeIsFileException("Path '" + node.getPath() + "' is of type FILE.");
        }
        
        Collection<PathTreeNode<V>> childrenValues = node.getChildren().values();
        for(PathTreeNode<V> childNode : childrenValues)
        {
            /*if(!childNode.isLogicallyDeleted())
            {
                registerDeletePath(childNode.getPath(), force);
            }*/
            try
            {
                registerDeletePath(childNode.getPath(), force);
            }
            catch (NoSuchNodeException e)
            {
                // this means that the node is already marked for deletion
            }
        }
    }
    
    /**
     * Registers the change of a property of <code>path</code> or of the subtree rooted
     * at <code>path</code> (if <code>recurse</code> is true).
     *
     * @param path the path of the node whose property you want to set
     * @param property the name of the property you want to set
     * @param value the value of the property you want to set
     * @param recurse if you want to set this property not only for <code>path</code>,
     *                but also for all the ancestors of <code>path</code>, then set
     *                this to true
     * @throws NodeIsFileException if some proper subpath of <code>path</code>
     *                             indicates a file
     * @throws NoSuchNodeException if <code>path</code> doesn't point to a valid node
     */
    public void registerSetProperty(String path, String property, String value, boolean recurse)
        throws NodeIsFileException, NoSuchNodeException
    {
        path = Path.removeSlashes(path);
        PathTreeNode<V> node = getNode(path);
        if (node != null)
        {
            registerSetProperty(node, property, value, recurse);
        }
        else
        {
            throw new NoSuchNodeException("Path '" + path + "' does not indicate a valid node.");
        }

        _fixed = true;
    }
    
    public void registerCopyFile(String oldPath, String newPath)
    	throws NoSuchNodeException, NodeExistsException, NodeIsFileException
    {
        registerCopyFile(oldPath, newPath, getRevision());
    }
    
    /**
     * registerCopyFile registers a file or directory to copied on the next commit
     * the file or directory or any of it's children have to have no local modifications
     * the copy will create a local copy that can be edited before the commit
     * when getting a file in the copy before the commit they will be redirected to 
     * the original one (in the repositry)
     *
     * @param oldPath the path of the file or directory to copy
     * @param newPath the path to copy the file or directory to
     * @param revision the revision that will be copied
     * @throws NodeIsFileException 
     * @throws NoSuchNodeException if <code>oldPath</code> doesn't point to a valid node
     * @throws NodeExistsException if <code>newPath</code> points to node
     */
    public void registerCopyFile(String oldPath, String newPath, Long revision)
    	throws NoSuchNodeException, NodeExistsException, NodeIsFileException
{
    System.out.println("registerCopyFile("+oldPath+","+newPath+","+revision+")");
    
    //boolean copyToDir = newPath.endsWith("/");
    oldPath = Path.removeSlashes(oldPath);
    String origPath = getRealPath(oldPath);
    
    
    String[] oldPathElems = Path.removeSlashes(oldPath).split("/");
    String oldFileName = oldPathElems[oldPathElems.length -1];
    
    //If the newPath ends with a / then we now that what ever we copy
    //has to be placed in the dir so we append the file or dir name
    //of the old one
    if(newPath.endsWith("/"))
    {
        newPath +=  oldFileName;
    }
    
    newPath = Path.removeSlashes(newPath);
    
    String[] newPathElems = Path.removeSlashes(newPath).split("/");
    String newFileName = newPathElems[newPathElems.length -1];
    
    PathTreeNode<V> oldNode = getNode(oldPath);
    PathTreeNode<V> newNode = null; //getNode(newPath);
    PathTreeNode<V> newClone = null;
    
    //We might want to dissalow the copying of a already copied file.
    //if(oldNode.isPartOfCopy())
    //{
    //    throw new NodeExistsException("File or directory is part of an earlier copy or move please commit those changes first.");
    //}
    
    if(oldNode != null)
    {
                 
        
        CommitItem changes = oldNode.getChanges();
        
        boolean isAdd = false;
        boolean isMod = false;
        boolean isCopy = oldNode.isPartOfCopy();
        boolean isDir  = (oldNode.getType() == SVNNodeKind.DIR);
        
        if(changes != null)
        {
            isAdd  = changes.isAdd();
            isMod  = (changes.isModFile() || changes.isModProperty());
            //isCopy = changes.isCopy();
            
        }
        
        //If it is a dir and no modifications
        //check if any of the children has modifications
        if(isDir && !isMod)
        {
            Collection<CommitItem> ci = getCommitItems(oldNode);
            if(ci != null)
            {
            	isMod = (ci.size() != 0);
            }
        }
        
        if((!isAdd || isCopy)  && !isMod)
        {
            CommitItem newChanges = new CommitItem(isDir, newPath);
            try
            {
                newClone = (PathTreeNode<V>)oldNode.clone();
                //Using a bit of a dirty hack here
                //Inserting a new node
                //then deleting it 
                //and adding the clone in it's place
                insertNode(newPath, oldNode.getType(), null);                
                newNode = getNode(newPath);
                PathTreeNode<V> parent = newNode.getParent();
                parent.deleteChild(newFileName);
                parent.addChild(newFileName, newClone);
                //System.out.println("blaat");
               
                
            }
            catch(CloneNotSupportedException e)
            {
                throw new NoSuchNodeException("Error while cloning the original node");
            }
            catch(Exception e)
            {
                throw new NodeExistsException("Desitnation already exists");
            }
            
            if(isCopy)
            {
                newChanges.markAsCopy(changes.getCopyFromPath(),
                                      revision);
            }
            else
            {
                newChanges.markAsCopy(origPath, revision);
            }
            
            //newNode = getNode(newPath); 
            rebuildTree(newClone, newPath);
            newClone.setChanges(newChanges);
            
        }
        else
        {
            throw new NoSuchNodeException("File or directory to be copied has local changes, commit these changes first.");
        }
        
    }
    else
    {
        throw new NoSuchNodeException("Old path doesn't exist");
    }
    
}
    
    /**
     * registerMoveFile registers a file or directory to moved on the next commit
     * the file had to have no local modifications
     * a directory has to have no local modificatiosn or any children with
     * local modifictions
     *
     * @param oldPath the path of the file or directory to copy
     * @param newPath the path to copy the file or directory to
     * @param force this will force the move even if there are local modifications
     * @throws NodeIsFileException 
     * @throws NoSuchNodeException if <code>oldPath</code> doesn't point to a valid node
     * @throws NodeExistsException if <code>newPath</code> points to node
     */    
    public void registerMoveFile(String oldPath, String newPath, boolean force)
        throws NoSuchNodeException, NodeExistsException, NodeIsFileException
    {
        
        oldPath = Path.removeSlashes(oldPath);
        
        
        String[] oldPathElems = Path.removeSlashes(oldPath).split("/");
        String oldFileName = oldPathElems[oldPathElems.length -1];
        
//      If the newPath ends with a / then we now that what ever we move
        //has to be placed in the dir so we append the file or dir name
        //of the old one
        if(newPath.endsWith("/"))
        {
            newPath +=  oldFileName;
        }
        
        newPath = Path.removeSlashes(newPath);
        
        String[] newPathElems = Path.removeSlashes(newPath).split("/");
        String newFileName = newPathElems[newPathElems.length -1];
        
        PathTreeNode<V> oldNode = getNode(oldPath);
        PathTreeNode<V> newNode = null; //getNode(newPath);
        
        //Might want to dissalow moving of locally changed stuff
        //if(oldNode.isPartOfCopy())
        //{
        //    throw new NodeExistsException("File or directory is part of an earlier copy or move please commit those changes first.");
        //}
        
        if(oldNode != null)
        {
            
            //boolean isDir = (oldNode.getType() == SVNNodeKind.DIR);           
            
            CommitItem changes = oldNode.getChanges();
            boolean isAdd = false;
            boolean isMod = false;
            boolean isCopy = oldNode.isPartOfCopy();
            boolean isDir  = (oldNode.getType() == SVNNodeKind.DIR);
            
            if(changes != null)
            {
                isAdd  = changes.isAdd();
                isMod  = (changes.isModFile() || changes.isModProperty());
                //isCopy = changes.isCopy() || oldNode.isPartOfCopy();
                
            }
            /*else
            {
            	changes = new CommitItem(isDir,oldPath);
            }*/
            
            if(isDir && !isMod)
            {
                Collection<CommitItem> ci = getCommitItems(oldNode);
                if(ci != null)
                {
                	isMod = (ci.size() != 0);
                }
            }
            
            if((!isAdd && !isMod && !isCopy) || force)
            {
                PathTreeNode<V> oldParent = null;
                PathTreeNode<V> newParent = null;
                try
                {
                    insertNode(newPath, oldNode.getType(), null);
                    newNode = getNode(newPath);
                    
                    oldParent = oldNode.getParent();
                    newParent = newNode.getParent();                    
                    
                    oldParent.deleteChild(oldFileName);                       
                    oldParent.addChild(oldFileName, newNode);                    
                    
                    //ok
                    
                    
                    newParent.deleteChild(newFileName);
                    newParent.addChild(newFileName, oldNode);                    
                    
                    
                    //changes.setPath(newPath);
                    newNode.setParent(oldParent);
                    oldNode.setParent(newParent);
                    //ok
                    
                    rebuildTree(oldNode, newPath);
                    rebuildTree(newNode, oldPath);
                    
                }
                catch(Exception e)
                {
                	e.printStackTrace();
                    throw new NodeExistsException("Trying to move to a location that already exists");
                }
                
                //System.out.println(newNode.getParent()); 
//              
                if(!isAdd)
                {
                    //System.out.println("don;t go here");
                    if(changes != null)
                    {
                        //If there were no changes before there are now
                        //this file is copied an moved
                        changes.markAsCopy(oldPath,getRevision());
                        changes.markAsMove(oldPath);
                    }
                    else
                    {
                        //add the copy and moved flags
                        CommitItem newChanges = new CommitItem(isDir,newPath);
                        newChanges.markAsCopy(oldPath,getRevision());
                        newChanges.markAsMove(oldPath);
                        oldNode.setChanges(newChanges);
                    }
                }
                else
                {
                    
                    //System.out.println("go here");
                    //If this i locally added file or dir
                    //we should not mark it as a copy
                    //but we have to keep the added status
                    //on the oldPath so that deletion goes as expected
                    
                    changes.markAsMove(oldPath);
                    //System.out.println(changes.getPath());
                    CommitItem newChanges = new CommitItem(isDir,oldPath);
                    newChanges.markAsAdd();
                    newNode.setChanges(newChanges);
                }
                
                
                //rebuildTree(oldNode, newPath);
                //rebuildTree(newNode, oldPath);
                
                
                
                registerDeletePath(oldPath, true);
                
                
            }
            else
            {
                throw new NoSuchNodeException("File or directory has local changes, use force = true");
            }
            
        }
        else
        {
            throw new NoSuchNodeException("No such file or directory");
        }        
    }

    public void revertFile(String path, long revision)
        throws NoSuchNodeException, NodeIsFileException
    {
        path = Path.removeSlashes(path);
        PathTreeNode<V> node = getNode(path);
        if(node == null)
        {
            throw new NoSuchNodeException(path + " does not exist");
        }
        
        CommitItem changes = node.getChanges();
        
        boolean isAdd = false;
        boolean isMod = false;
        boolean isCopy = false;
        boolean isDir  = (node.getType() == SVNNodeKind.DIR);
        
        if(changes != null)
        {
            isAdd  = changes.isAdd();
            isMod  = (changes.isModFile() || changes.isModProperty());
            isCopy = changes.isCopy();
            
        }
        
        if(isDir && !isMod)
        {
            Collection<CommitItem> ci = getCommitItems(node);
            isMod = (ci != null);
        }
        
        if(!isAdd && !isMod && !isCopy)
        {
            if(changes == null)
            {
                changes = new CommitItem(isDir, path);
                node.setChanges(changes);
            }
            changes.markAsDelete();
            changes.markAsCopy(path, revision);
            
        }
        else
        {
            throw new NoSuchNodeException("File or directory has local changes, commit those first");
        }
    }
    
    
    /**
     * Commits all the changes in this path tree. For performance reasons, you should
     * not call this method on the entire path tree, but instead create a reduced one
     * where all the leaves have commit items associated with them.
     *
     * @param editor the ISVNEditor to use for committing changes.
     */
    public void commit(ISVNEditor editor)
        throws SVNException, StorageException
    {
        commit(editor, _root);
        editor.closeEdit();
        //System.out.println("closeEdit()");
    }
    
    public void registerSuccessfulCommit()
    {
        clearCommitItems();
        _revision++;
        _fixed = false;
    }

    /**
     * Makes a report about the working copy, as specified by the JavaSVN update model.
     */
    public void report(ISVNReporter reporter, String repositoryURL) throws SVNException
    {
        if (!repositoryURL.endsWith("/"))
        {
            repositoryURL += "/";
        }

        report(_root, reporter, repositoryURL);

        reporter.finishReport();
    }

    /**
     * Adds a new node in the tree. If a node with the same <code>path</code>
     * exists in the tree, a NodeExistsException will be thrown, unless the existing
     * node is marked for deletion (by means of the commit item associated with it)
     * <b>and</b> not marked for (re)addition (by means of the same commit item). In
     * this case, the existing node will be kept unchanged and its associated commit
     * item will be marked for readdition.
     *
     * @param path the path of the node to insert
     * @param type the type of node this path represents (SVNNodeKind.DIR or
     *             SVNNodeKind.FILE)
     * @param value the value you want to associate with the node (null allowed)
     * @throws NodeIsFileException if some subpart of the path is a file in the current
     *                             tree. This would mean that you are trying to insert
     *                             a node which would (directly or indirectly) be the
     *                             child of a file, which is not allowed
     * @throws NodeExistsException if the path you are trying to insert already exists
     *                             in the tree 
     */
    private void insertNode(String path, SVNNodeKind type, V value)
        throws NodeIsFileException, NodeExistsException
    {
        insertNode(new PathTreeNodeData<V>(path, type, value), false);
    }

    /**
     * Equivalent to insertNode(new PathTreeNodeData<V>(path, type, value)). See
     * {@link #insertNode(String, SVNNodeKind, V)} for details.
     */
    private void insertNode(PathTreeNodeData<V> nodeData)
        throws NodeIsFileException, NodeExistsException
    {
        insertNode(nodeData, false);
    }

    /**
     * Internal version of insertNode. See {@link #insertNode(PathTreeNodeData)} for
     * details.
     *
     * @param register if this is set to true, then all the changes made to the tree
     *                 are registered by associating commit items with the inserted
     *                 nodes
     * @return the inserted node
     */
    private PathTreeNode<V> insertNode(PathTreeNodeData<V> nodeData, boolean register)
        throws NodeIsFileException, NodeExistsException
    {
        if (_root == null)
        {
            _root = new PathTreeNode<V>("", SVNNodeKind.DIR, _rootValue);
        }

        // split the path into its composing elements
        String[] pathElems = Path.removeSlashes(nodeData.getPath()).split("/");
        if ((pathElems.length == 0) ||
                (pathElems.length == 1 && pathElems[0].equals("")))
        {
            return _root;
        }

        PathTreeNode<V> curNode = _root;
        for (int i = 0; i < pathElems.length; i++)
        {
            String pathElem = pathElems[i];
            PathTreeNode<V> node = curNode.getChild(pathElem);

            if ((i == pathElems.length - 1) || (node == null))
            {
                PathTreeNode<V> newNode;

                // if this node was deleted and then it wasn't readded, we can then
                // allow it to be added
                if (node != null && node.isLogicallyDeleted())
                {
                    node.getChanges().markAsAdd();
                    node.getChanges().setBaseRevision(_revision);
                    newNode = node;
                }
                else    // else we just try to add the child
                        // if the node exists, then an exception will be thrown
                {
                    newNode = new PathTreeNode<V>(curNode.getPath() +
                            (curNode.getPath().equals("") ? "" : "/") + pathElem,
                            SVNNodeKind.DIR, null);
                    curNode.addChild(pathElem, newNode);
                    if (register)
                    {
                        newNode.setChanges(new CommitItem(true, newNode.getPath(), true));
                        newNode.getChanges().setBaseRevision(_revision);
                    }
                }

                curNode = newNode;
            }
            else
            {
                curNode = node;

                // if the directory was deleted and hasn't been readded,
                // then we readd it
                if (node.isLogicallyDeleted())
                {
                    node.getChanges().markAsAdd();
                    node.getChanges().setBaseRevision(_revision);
                }
            }
        }

        curNode.setType(nodeData.getType());
        curNode.setValue(nodeData.getValue());

        return curNode;
    }

    /**
     * Deletes the node identified by <code>path</code>.
     *
     * @param path the path of the node to delete
     * @return the value associated with the deleted node or null if either
     *         the node didn't exist or if the value associated with it was null.
     */
    private V deleteNode(String path) throws NodeIsFileException
    {
        String[] pathElems = Path.removeSlashes(path).split("/");
        if (pathElems.length == 1 && pathElems[0].equals(""))
        {
            pathElems = new String[0];
        }

        int index = 0;
        PathTreeNode<V> curNode = _root;
        while (curNode != null && index < pathElems.length - 1)
        {
            curNode = curNode.getChild(pathElems[index]);
            index++;
        }

        if (curNode != null && index == pathElems.length - 1)
        {
            PathTreeNode<V> node = curNode.deleteChild(pathElems[index]);
            return (node == null) ? null : node.getValue();
        }
        else if (curNode != null && curNode == _root)
        {
            _root = null;
            return curNode.getValue();
        }
        else
        {
            return null;
        }
    }
    
    private void rebuildTree(PathTreeNode<V> node, String basePath)
    {
        node.setPath(basePath);
        CommitItem changes = node.getChanges();
        if(changes != null)
        {
            changes.setPath(basePath);
        }
        
        Map<String, PathTreeNode<V>> children = node.getChildren();
        for(Map.Entry<String, PathTreeNode<V>> entry : children.entrySet())
        {
            String tmpPath = basePath + "/" + entry.getKey();
            rebuildTree(entry.getValue(), tmpPath);
            
        }
    }

    /**
     * Returns the node identified by <code>path</code> or null, if no such node exists.
     * This method takes the commit items associated with nodes into account. If
     * anywhere along the <code>path</code>, there is a node marked for deletion and
     * not remarked for addition, then null will be returned (since theoretically the
     * node specified by path no longer exists). 
     *
     * @param path the path of the node you want
     * @throws NodeIsFileException if some proper subpath of <code>path</code>
     *                             indicates a file
     */
    private PathTreeNode<V> getNode(String path) throws NodeIsFileException
    {
        return getNode(path,true);
        /*String[] pathElems = Util.removeSlashes(path).split("/");
        if (pathElems.length == 1 && pathElems[0].equals(""))
        {
            pathElems = new String[0];
        }

        int index = 0;
        PathTreeNode<V> curNode = _root;
        while (curNode != null && index < pathElems.length)
        {
            curNode = curNode.getChild(pathElems[index]);
            
            if (curNode != null && curNode.isLogicallyDeleted())
            {
                curNode = null;
            }
                
            index++;
        }

        return curNode;*/
    }
    
    private PathTreeNode<V> getNode(String path, boolean dontReturnLogicallyDeletedNodes)
        throws NodeIsFileException
    {
        String[] pathElems = Path.removeSlashes(path).split("/");
        if (pathElems.length == 1 && pathElems[0].equals(""))
        {
            pathElems = new String[0];
        }

        int index = 0;
        PathTreeNode<V> curNode = _root;
        while (curNode != null && index < pathElems.length)
        {
            curNode = curNode.getChild(pathElems[index]);
            
            if (dontReturnLogicallyDeletedNodes && curNode != null && curNode.isLogicallyDeleted())
            {
                curNode = null;
            }

            index++;
        }

        return curNode;
    }
    
    /**
     * Returns a Map with all the paths and corresponding values of the children or descendents
     * of <code>node</code>.
     * 
     * @param node the node whose children's (and possibly descendents') values you want
     * @param recurse set to true if you want the values of all the descendents of <code>node</code>
     *                If set to false, then only its direct children are returned.
     * @return a Map of paths with their corresponding values.
     */
    private Map<String, V> getNodeValues(PathTreeNode<V> node, boolean recurse)
    {
        HashMap<String, V> result = new HashMap<String, V>();
        for (PathTreeNode<V> childNode : node.getChildren().values())
        {
            if (!childNode.isLogicallyDeleted())
            {
                result.put("/" + childNode.getPath(), childNode.getValue());
                if (recurse)
                {
                    result.putAll(getNodeValues(childNode, recurse));
                }
            }
        }

        return result;
    }

    /**
     * Internal version of {@link #getConflicts()}
     *
     * @param node the start node
     */
    private Collection<String> getConflicts(PathTreeNode<V> node)
    {
        ArrayList<String> result = new ArrayList<String>();

        if (node.isInConflict())
        {
            result.add(node.getPath());
        }

        if (!node.isLogicallyDeleted())
        {
            for (PathTreeNode<V> entry : node.getChildren().values())
            {
                result.addAll(getConflicts(entry));
            }
        }

        return result;
    }

    /**
     * Internal version of {@link #getCommitItems()}
     *
     * @param node the start node
     */
    private Collection<CommitItem> getCommitItems(PathTreeNode<V> node)
    {
        ArrayList<CommitItem> result = new ArrayList<CommitItem>();

        if (node.getChanges() != null)
        {
            result.add(node.getChanges());
        }

        if (!node.isLogicallyDeleted())
        {
            for (PathTreeNode<V> entry : node.getChildren().values())
            {
                result.addAll(getCommitItems(entry));
            }
        }

        return result;
    }

    /**
     * Goes through the entire tree and removes commit items associated with any node.
     * All commit items that indicate deletion of nodes determine the deletion of their
     * associated nodes.
     * <br /><br />
     * This method is presumably called after a successful commit in order to indicate
     * that the commit items should be performed and removed.
     */
    private void clearCommitItems()
    {
        clearCommitItems(_root);
    }

    /**
     * Internal version of {@link #clearCommitItems()}.
     *
     * @param node the start node
     */
    private void clearCommitItems(PathTreeNode<V> node)
    {
        if (node != null)
        {
            node.setChanges(null);
            ArrayList<String> deleteNames = new ArrayList<String>();
            for (Map.Entry<String, PathTreeNode<V>> childNode : node.getChildren().entrySet())
            {
                if (childNode.getValue().isLogicallyDeleted())
                {
                    deleteNames.add(childNode.getKey());
                }
                else
                {
                    clearCommitItems(childNode.getValue());
                }
            }
            for (String deleteName : deleteNames)
            {
                try
                {
                    node.deleteChild(deleteName);
                }
                catch (Exception e)
                {
                    // this shouldn't be thrown if the tree is in a valid state
                    e.printStackTrace();
                }
            }
        }
    }

    /**
     * Internal version of {@link #registerSetProperty(String, String, String, String)}.
     */
    private void registerSetProperty(PathTreeNode<V> node, String property, String value,
            boolean recurse)
    {
        if (node != null && !node.isLogicallyDeleted())
        {
            if (node.getChanges() == null)
            {
                node.setChanges(new CommitItem(node.getType() == SVNNodeKind.DIR,
                        node.getPath()));
            }
            node.getChanges().setProperty(property, value);
            node.getChanges().setBaseRevision(_revision);

            if (recurse)
            {
                for (PathTreeNode<V> childNode : node.getChildren().values())
                {
                    registerSetProperty(childNode, property, value, recurse);
                }
            }
        }
    }

    /**
     * Internal version of {@link #commit(ISVNEditor)}.
     */
    public void commit(ISVNEditor editor, PathTreeNode<V> node)
        throws SVNException, StorageException
    {
        CommitItem item = node.getChanges();
        if (item != null)
        {
            item.commit(editor);
        }

        if (node.getType() == SVNNodeKind.DIR)
        {
            if (item == null)
            {
                if (node.getPath().equals(""))
                {
                    editor.openRoot(getRevision());
                    //System.out.println("openRoot(" + getRevision() + ")");
                }
                else
                {
                    editor.openDir(node.getPath(), getRevision());
                    //System.out.println("openDir(" + node.getPath() + ", " + getRevision() + ")");
                }
            }

            for (PathTreeNode<V> child : node.getChildren().values())
            {
                commit(editor, child);
            }

            if (item == null || !node.isLogicallyDeleted())
            {
                editor.closeDir();
                //System.out.println("closeDir()");
            }
        }
    }
    
    /**
     * Internal version of {@link #report(ISVNReporter, String)}.
     */
    private void report(PathTreeNode<V> node, ISVNReporter reporter, String repositoryURL)
        throws SVNException
    {
        if (node != null)
        {
            if (node.getChanges() == null)
            {
                reporter.setPath(node.getPath(), null, _revision, false);
            }
            else
            {
                CommitItem item = node.getChanges();
                if (item.isDelete())
                {
                    reporter.deletePath(node.getPath());
                }
                
                // For some reason, if files are reported as in the parts commented
                // below, update doesn't work correctly anymore (i.e., abortEdit() is
                // called). According to the specifications, this shouldn't be the case,
                // but reality shows otherwise. So, for now, hopefully update works
                // correctly even without reporting locally added/copied files.
                
                else if (item.isCopy())
                {
                    //String copyPath = repositoryURL + item.getCopyFromPath();
                    //reporter.linkPath(SVNURL.parseURIEncoded(copyPath), node.getPath(),
                    //        null, _revision, false);
                }
                else if (item.isAdd())
                {
                    //reporter.setPath(node.getPath(), null, _revision, false);
                }
                else
                {
                    //reporter.setPath(node.getPath(), null, _revision, false);
                }
            }
            if (!node.isLogicallyDeleted())
            {
                for (PathTreeNode<V> childNode : node.getChildren().values())
                {
                    report(childNode, reporter, repositoryURL);
                }
            }
        }
    }

    /***************************************************
     * The methods that follow are ISVNEditor methods. *
     ***************************************************/

    public void abortEdit() throws SVNException
    {
        //System.out.println("abortEdit() called.");
    }

    public void absentDir(String path) throws SVNException
    {
        //System.out.println("absentDir(" + path + ") called.");
    }

    public void absentFile(String path) throws SVNException
    {
        //System.out.println("absentFile(" + path + ") called.");
    }

    public void addDir(String path, String copyFromPath, long copyFromRevision) throws SVNException
    {
        //System.out.println("addDir(" + path + ", " + copyFromPath + ", " +
        //        copyFromRevision + ") called.");

        try
        {
            if (shouldAdd(path))
            {
                insertNode(path, SVNNodeKind.DIR, null);
            }
        }
        catch (Exception e)
        {
            throw new SVNException(e.getMessage());
        }
    }

    public void addFile(String path, String copyFromPath, long copyFromRevision) throws SVNException
    {
        //System.out.println("addFile(" + path + ", " + copyFromPath + ", " +
        //        copyFromRevision + ") called.");

        try
        {
            if (shouldAdd(path))
            {
                insertNode(path, SVNNodeKind.FILE, null);
            }
        }
        catch (Exception e)
        {
            throw new SVNException(e.getMessage());
        }
    }

    public void applyTextDelta(String path, String baseChecksum) throws SVNException
    {
        //System.out.println("applyTextDelta(" + path + ", " + baseChecksum + ") called.");

        try
        {
            CommitItem item = getNodeChanges(path);
            if (item != null && item.isModFile())
            {
                _applyChanges = true;
            }
            else
            {
                _applyChanges = false;
            }
        }
        catch (Exception e)
        {
            _applyChanges = false;
            throw new SVNException(e.getMessage());
        }
    }

    public void changeDirProperty(String name, String value) throws SVNException
    {
        //System.out.println("changeDirProperty(" + name + ", " + value + ") called.");
    }

    public void changeFileProperty(String path, String name, String value) throws SVNException
    {
        //System.out.println("changeFileProperty(" + path + ", " + name + ", " +
        //       value + ") called.");
    }

    public void closeDir() throws SVNException
    {
        //System.out.println("closeDir() called.");
    }

    public SVNCommitInfo closeEdit() throws SVNException
    {
        //System.out.println("closeEdit() called.");

        assert _targetRevision != -1;

        _revision = _targetRevision;
        for (CommitItem item : getCommitItems())
        {
            item.setBaseRevision(_revision);
        }

        _targetRevision = -1;

        return null;
    }

    public void closeFile(String path, String textChecksum) throws SVNException
    {
        //System.out.println("closeFile(" + path + "," + textChecksum + ") called.");
    }

    public void deleteEntry(String path, long revision) throws SVNException
    {
        //System.out.println("deleteEntry(" + path + "," + revision + ") called.");
    }

    public void openDir(String path, long revision) throws SVNException
    {
        //System.out.println("openDir(" + path + "," + revision + ") called.");
    }

    public void openFile(String path, long revision) throws SVNException
    {
        //System.out.println("openFile(" + path + "," + revision + ") called.");
    }

    public void openRoot(long revision) throws SVNException
    {
        //System.out.println("openRoot(" + revision + ") called.");

        _mediator = new SVNRAWorkspaceMediator();
    }

    public void targetRevision(long revision) throws SVNException
    {
        //System.out.println("targetRevision(" + revision + ") called.");

        _targetRevision = revision;
    }

    public OutputStream textDeltaChunk(String path, SVNDiffWindow diffWindow) throws SVNException
    {
        //System.out.println("textDeltaChunk(" + path + "," + diffWindow + ") called.");

        if (_applyChanges)
        {
            assert _mediator != null;
            
            if (_diffWindows == null)
            {
                _diffWindows = new LinkedList<SVNDiffWindow>();
            }
            
            _diffWindows.add(diffWindow);
            
            try
            {
                return _mediator.createTemporaryLocation(path, diffWindow);
            }
            catch (IOException e)
            {
                throw new SVNException(e);
            }
        }
        else
        {
            return null;
        }
    }

    public void textDeltaEnd(String path) throws SVNException
    {
        //System.out.println("textDeltaEnd(" + path + ") called.");

        if (_applyChanges)
        {
            PathTreeNode<V> node = null;
            try
            {
                node = getNode(path);
            }
            catch (Exception e)
            {
                throw new SVNException(e.getMessage());
            }

            assert node != null;
            File baseFile = node.getBaseFile();
            assert baseFile != null;

            File newBaseFile = null;
            try
            {
                newBaseFile = File.createTempFile("gw-", "");
                _tempFiles.add(newBaseFile);
            }
            catch (IOException e)
            {
                throw new SVNException(e.getMessage());
            }

            // first, update the base file of this node

            // a null _diffWindows means empty file
            if (_diffWindows != null)
            {
                SVNDiffWindowApplyBaton applyBaton;
                applyBaton = SVNDiffWindowApplyBaton.create(baseFile, newBaseFile, null);

                for (SVNDiffWindow diffWindow : _diffWindows)
                {
                    if (diffWindow != null)
                    {
                        InputStream newData = null;

                        try
                        {
                            newData = _mediator.getTemporaryLocation(diffWindow);
                            diffWindow.apply(applyBaton, newData);
                        }
                        catch (IOException e)
                        {
                            throw new SVNException(
                                    "Error while fetching a temporary delta storage.");
                        }
                        finally
                        {
                            _mediator.deleteTemporaryLocation(diffWindow);
                        }
                    }
                }

                applyBaton.close();
                _diffWindows.clear();
                _diffWindows = null;
            }

            // now, merge the changes

            ISVNMergerFactory mergerFactory;
            mergerFactory = SVNWCUtil.createDefaultOptions(true).getMergerFactory();
//            byte[] conflictStart = ("<<<<<<< local file").getBytes();
//            byte[] conflictEnd = (">>>>>>> repository file").getBytes();
//            byte[] separator = ("=======").getBytes();
	          byte[] conflictStart = ("<para>local file</para>").getBytes();
	          byte[] conflictEnd = ("</para>repos file</para>").getBytes();
	          byte[] separator = ("=======").getBytes();

            ISVNMerger merger = mergerFactory.createMerger(conflictStart, separator, conflictEnd, null);

            try
            {
                File oldFile = node.getChanges().getFile();
                File newFile = File.createTempFile("gw-", "");
                _tempFiles.add(newFile);
                OutputStream os = new BufferedOutputStream(new FileOutputStream(newFile));

                SVNStatusType mergeStatus;
                mergeStatus = merger.mergeText(baseFile, oldFile, newBaseFile, false, os);
                os.close();
                if (mergeStatus == SVNStatusType.CONFLICTED)
                {       
                    node.setInConflict(true);
                }

                newFile.renameTo(oldFile);
                newBaseFile.renameTo(baseFile);
            }
            catch (IOException e)
            {
                e.printStackTrace();
            }
        }
    }

    /**
     * Sometimes addDir/addFile is called for nodes that have been marked for deletion
     * locally. In this case, the dir/file shouldn't be readded. This methods checks
     * for such cases and returns true or false accordingly.
     */
    private boolean shouldAdd(String path) throws NodeIsFileException
    {
        String[] pathElems = Path.removeSlashes(path).split("/");
        if (pathElems.length == 0 || (pathElems.length == 1 && pathElems[0].equals("")))
        {
            return !_root.isLogicallyDeleted();
        }

        int index = 0;
        PathTreeNode<V> curNode = _root;
        while (curNode != null && index < pathElems.length)
        {
            curNode = curNode.getChild(pathElems[index]);
            if (curNode != null && curNode.isLogicallyDeleted())
            {
                return false;
            }

            index++;
        }

        return true;
    }

    @Override
    protected void finalize() throws Throwable
    {
        for (File f: _tempFiles)
        {
            f.delete();
        }
    }

    private List<File> _tempFiles;
    private long _targetRevision;
    private SVNRAWorkspaceMediator _mediator;
    private LinkedList<SVNDiffWindow> _diffWindows;
    private boolean _applyChanges;
}

class PathTreeNode<V> implements Cloneable
{
    private String _path;
    private SVNNodeKind _type;
    private V _value;

    private CommitItem _changes;
    private PathTreeNode<V> _parent;
    private HashMap<String, PathTreeNode<V>> _children;

    /**
     * If this node contains modifications to a file, then in order to
     * update that file, its base version  is needed.
     */
    private File _baseFile;
    private boolean _inConflict;

    public PathTreeNode(String path, SVNNodeKind type, V value)
    {
        _path = path;
        _type = type;
        _value = value;

        _changes = null;
        _parent = null;
        _children = new HashMap<String, PathTreeNode<V>>();

        _baseFile = null;
        _inConflict = false;
    }

    public String getPath()
    {
        return _path;
    }

    public void setPath(String path)
    {
        _path = path;
    }

    public SVNNodeKind getType()
    {
        return _type;
    }

    /**
     * Be aware that changing a node's type from DIR to FILE will also imply that all
     * the children of the node are deleted.
     */
    public void setType(SVNNodeKind type)
    {
        if (_type == SVNNodeKind.DIR && type == SVNNodeKind.FILE)
        {
            _children.clear();
        }

        _type = type;
    }

    public V getValue()
    {
        return _value;
    }

    public void setValue(V value)
    {
        _value = value;
    }

    public PathTreeNode<V> getParent()
    {
        return _parent;
    }

    public void setParent(PathTreeNode<V> parent)
    {
        _parent = parent;
    }

    public CommitItem getChanges()
    {
        return _changes;
    }

    public void setChanges(CommitItem changes)
    {
        _changes = changes;
    }

    public File getBaseFile()
    {
        return _baseFile;
    }

    public void setBaseFile(File baseFile)
    {
    	    _baseFile = baseFile;
    }

    public boolean isInConflict()
    {
        return _inConflict;
    }

    public void setInConflict(boolean inConflict)
    {
        _inConflict = inConflict;
    }

    public boolean isLogicallyDeleted()
    {
        return _changes != null && _changes.isDelete() && !_changes.isAdd();
    }
    
    public boolean isPartOfCopy()
    {
        if((_changes != null) && (_changes.isCopy()))
        {
            return true;
        }
        else if(_parent != null)
        {   
            return _parent.isPartOfCopy();                
            
        }
        else
        {
            return false;
        }
    }

    /**
     * Returns the child identified by <code>name</code>.
     */
    public PathTreeNode<V> getChild(String name) throws NodeIsFileException
    {
        assert !isLogicallyDeleted();

        if (_type == SVNNodeKind.FILE)
        {
            throw new NodeIsFileException("Path '" + _path + "' is of type FILE.");
        }

        return _children.get(name);
    }

    /**
     * Adds a child to this node. If a child with the same name already exists,
     * the old child will be replaced with the new one.
     */
    public void addChild(String name, PathTreeNode<V> child)
        throws NodeIsFileException, NodeExistsException
    {
        assert !isLogicallyDeleted();

        if (_type == SVNNodeKind.FILE)
        {
            throw new NodeIsFileException("Path '" + _path + "' is of type FILE.");
        }
        if (_children.containsKey(name))
        {
            throw new NodeExistsException("Path '" + _path + "/" + name + "' already exists.");
        }

        _children.put(name, child);
        child.setParent(this);
    }

    /**
     * Deletes the child called <code>name</code> and returns it. If no child
     * with such a name exists, it returns null.
     */
    public PathTreeNode<V> deleteChild(String name) throws NodeIsFileException
    {
        assert !isLogicallyDeleted();

        if (_type == SVNNodeKind.FILE)
        {
            throw new NodeIsFileException("Path '" + _path + "' is of type FILE.");
        }

        PathTreeNode<V> node = _children.remove(name);
        if (node != null)
        {
            node.setParent(null);
        }
        
        return node;
    }

    /**
     * Returns all the children of this node.
     */
    public Map<String, PathTreeNode<V>> getChildren()
    {
        assert !isLogicallyDeleted();

        return _children;
    }

    /**
     * Deletes all the children of this node.
     */
    public void deleteAllChildren() throws NodeIsFileException
    {
        assert !isLogicallyDeleted();

        if (_type == SVNNodeKind.FILE)
        {
            throw new NodeIsFileException("Path '" + _path + "' is of type FILE.");
        }

        _children.clear();
    }

    @Override
    public Object clone() throws CloneNotSupportedException
    {
        PathTreeNode<V> dolly = new PathTreeNode<V>(_path, _type, _value);

        dolly._changes = null;
        dolly._parent = null;
        dolly._baseFile = null;
        dolly._inConflict = false;   

        dolly._children = new HashMap<String, PathTreeNode<V>>();
        for(Map.Entry<String, PathTreeNode<V>> entry : _children.entrySet())
        {
            PathTreeNode<V> childClone = (PathTreeNode<V>) entry.getValue().clone();
            dolly._children.put(entry.getKey(), childClone);
            childClone.setParent(dolly);
        }

        return dolly;
    }
}
