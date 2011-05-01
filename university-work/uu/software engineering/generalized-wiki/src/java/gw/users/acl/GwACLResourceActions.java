package gw.users.acl;

/**
 * The available resource actions in GW. 
 * @see ACLResourceAction
 */
public abstract class GwACLResourceActions {
    public static final ACLResourceAction BLAME_ACTION              = new NamedACLResourceAction(new ACLPermission[] { GwACLPermissions.READ_PERMISSION           });
    public static final ACLResourceAction EDIT_ACTION               = new NamedACLResourceAction(new ACLPermission[] { GwACLPermissions.WRITE_PERMISSION          });
    public static final ACLResourceAction RETRIEVE_ACTION           = new NamedACLResourceAction(new ACLPermission[] { GwACLPermissions.READ_PERMISSION           });
    public static final ACLResourceAction BROWSE_ACTION             = new NamedACLResourceAction(new ACLPermission[] { GwACLPermissions.BROWSE_PERMISSION         });
    public static final ACLResourceAction CREATE_ACTION             = new NamedACLResourceAction(new ACLPermission[] { GwACLPermissions.WRITE_PERMISSION          });
    public static final ACLResourceAction CREATE_DIRECTORY_ACTION   = new NamedACLResourceAction(new ACLPermission[] { GwACLPermissions.WRITE_PERMISSION          });
    public static final ACLResourceAction COPY_ACTION               = new NamedACLResourceAction(new ACLPermission[] { GwACLPermissions.READ_PERMISSION           });
    public static final ACLResourceAction MOVE_ACTION               = new NamedACLResourceAction(new ACLPermission[] { GwACLPermissions.DELETE_PERMISSION         });
    public static final ACLResourceAction DELETE_ACTION             = new NamedACLResourceAction(new ACLPermission[] { GwACLPermissions.DELETE_PERMISSION         });
    public static final ACLResourceAction DIFF_ACTION               = new NamedACLResourceAction(new ACLPermission[] { GwACLPermissions.READ_PERMISSION           });
    public static final ACLResourceAction LOG_ACTION                = new NamedACLResourceAction(new ACLPermission[] { GwACLPermissions.READ_PERMISSION           });
    public static final ACLResourceAction EDIT_ACL_ACTION           = new NamedACLResourceAction(new ACLPermission[] { GwACLPermissions.ACL_WRITE_PERMISSION      });
    public static final ACLResourceAction EDIT_PROPERTY_ACTION      = new NamedACLResourceAction(new ACLPermission[] { GwACLPermissions.WRITE_PERMISSION          });
    public static final ACLResourceAction RETRIEVE_PROPERTY_ACTION  = new NamedACLResourceAction(new ACLPermission[] { GwACLPermissions.READ_PERMISSION           });
    public static final ACLResourceAction EDIT_PASSWORD_ACTION      = new NamedACLResourceAction(new ACLPermission[] { GwACLPermissions.PASSWORD_PERMISSION       });
}
