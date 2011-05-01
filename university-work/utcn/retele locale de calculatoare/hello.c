/* hello.c 
 * 
 * "Hello, world" - the loadable kernel module version. 
 *
 * Compile this with 
 *
 *          gcc -c -I/lib/modules/`uname -r`/build/include hello.c [-Wall]
 */

/* Declare what kind of code we want from the header files */
#define __KERNEL__         /* We're part of the kernel */
#define MODULE             /* Not a permanent part, though. */

/* Standard headers for LKMs */
#include <linux/modversions.h> 
#include <linux/module.h>
#include <linux/config.h>
#include <linux/netfilter_ipv4.h>
#include <linux/ip.h>
#include <linux/in.h>

#define _LOOSE_KERNEL_NAMES
    /* With some combinations of Linux and gcc, tty.h will not compile if
       you don't define _LOOSE_KERNEL_NAMES.  It's a bug somewhere.
    */
#include <linux/tty.h>      /* console_print() interface */

static unsigned int stupid_hook(unsigned int hook,
	struct sk_buff** pskb,
	const struct net_device *indev,
	const struct net_device *outdev,
	int (*okfn) (struct sk_buff*))
{
	
	printk("<packet-info>\n");
	printk("\t<packet-len>");
	printk("%d", (*pskb)->len);
	printk("</packet-len>\n");
	printk("\t<protocol-type>");
	switch ((((*pskb)->nh).iph)->protocol)
	{
		case IPPROTO_TCP:
		{
			printk("tcp");
			break;
		}
		case IPPROTO_ICMP:
		{
			printk("icmp");
			break;
		}
		case IPPROTO_UDP:
		{
			printk("udp");
			break;
		}
		default:
		{
			printk("unknown");
			break;
		}
	}
	printk("</protocol-type>\n");
	printk("</packet-info>\n");
	
	return NF_ACCEPT;
}

static struct nf_hook_ops stupid_ops = {{NULL, NULL}, stupid_hook, PF_INET, NF_IP_POST_ROUTING, 0};

/* Initialize the LKM */
int init_module()
{
	console_print("Hello, world - this is the kernel speaking\n");

	nf_register_hook(&stupid_ops);
	
	/* If we return a non zero value, it means that 
	 * init_module failed and the LKM can't be loaded 
	 */
	return 0;
}


/* Cleanup - undo whatever init_module did */
void cleanup_module()
{
	nf_unregister_hook(&stupid_ops);
	console_print("Short is the life of an LKM\n");
}
