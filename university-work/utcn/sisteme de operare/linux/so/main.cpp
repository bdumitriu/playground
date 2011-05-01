#include <iostream.h>
#include <iomanip.h>
#include <string.h>
#include <stdio.h>
#include "include/defs.h"
#include "include/driver/FileSystemDriver.h"

int main(void)
{
	HDDDriver* hdd = new HDDDriver();
	HDD* cache = new HDDDriverCache(hdd);

	try
	{
		FileSystemDriver* fsd = new FileSystemDriver();
		DirectoryListing* dl;

		cout << (int) fsd->initializeDriver() << endl;
		cout << (int) fsd->formatHDD() << endl;
		cout << (int) fsd->createFile("/xxx", NORMAL_FILE_TYPE) << endl;
		cout << (int) fsd->createFile("/xyz", FOLDER_FILE_TYPE) << endl;
		cout << (int) fsd->createFile("/xyz/abc", NORMAL_FILE_TYPE) << endl;
		cout << (int) fsd->createFile("/xde", NORMAL_FILE_TYPE) << endl;
		cout << (int) fsd->createFile("/xfrejk", NORMAL_FILE_TYPE) << endl;
		cout << (int) fsd->createFile("/xferjkg", NORMAL_FILE_TYPE) << endl;
		cout << (int) fsd->createFile("/xfejk", NORMAL_FILE_TYPE) << endl;
		cout << (int) fsd->createFile("/xgekz", NORMAL_FILE_TYPE) << endl;
		cout << (int) fsd->createFile("/ffxde", NORMAL_FILE_TYPE) << endl;
		cout << (int) fsd->createFile("/frexfrejk", NORMAL_FILE_TYPE) << endl;
		cout << (int) fsd->createFile("/x423ferjkg", NORMAL_FILE_TYPE) << endl;
		cout << (int) fsd->createFile("/x5345fejk", NORMAL_FILE_TYPE) << endl;
		cout << (int) fsd->createFile("/xvevgekz", NORMAL_FILE_TYPE) << endl;
		cout << (int) fsd->listFiles("/", &dl) << endl;

		for (unsigned long i = 0; i < dl->size(); i++)
		{
			cout << "name: " << dl->getEntry(i)->getEntryName() << endl;
			cout << "type: " << (int) dl->getEntry(i)->getEntryType() << endl;
			cout << "blocks: " << (int) dl->getEntry(i)->getEntrySizeInBlocks() << endl;
			cout << "bytes: " << (int) dl->getEntry(i)->getEntrySizeInBytes() << endl;
			DirectoryEntry* ent = dl->getEntry(i);
			Date* dt = ent->getEntryCreationDate();
			cout << "cr. date: " << (int) dt->getDay() << "/" << (int) dt->getMonth() << "/" << (int) dt->getYear() << endl;
			dt = ent->getEntryLastAccessDate();
			cout << "l.a. date: " << (int) dt->getDay() << "/" << (int) dt->getMonth() << "/" << (int) dt->getYear() << endl;
			dt = ent->getEntryLastUpdateDate();
			cout << "l.u. date: " << (int) dt->getDay() << "/" << (int) dt->getMonth() << "/" << (int) dt->getYear() << endl;
		}

/*
		hdd->initializeHDD();
		HDDFormatter::formatHDD(cache);

		Inode* rootInode = new Inode();
		rootInode->setValid(1);
		rootInode->setFileName("/", 1);
		rootInode->setFileNameSize(1);
		rootInode->setFileType(FOLDER_FILE_TYPE);

		DirectoryListing* dl = new DirectoryListing(2);
		dl->addEntry(new DirectoryEntry(rootInode, ROOT_DIR_FILEATTR));
		dl->addEntry(new DirectoryEntry(rootInode, ROOT_DIR_FILEATTR));

		Inode* fbInode = new Inode();
		fbInode->readFromHardDisk(cache, FILEATTR_AREA_START_ADDR+FREEBLOCKS_FILEATTR);
		BlockMap* map = new BlockMap(cache);

		rootInode->writeToHardDisk(cache, FILEATTR_AREA_START_ADDR+ROOT_DIR_FILEATTR);
		Directory::writeDirectory(new FileOutputStream(cache, rootInode,
			FILEATTR_AREA_START_ADDR+ROOT_DIR_FILEATTR, map), dl);

		delete dl;

		rootInode->readFromHardDisk(cache, FILEATTR_AREA_START_ADDR+ROOT_DIR_FILEATTR);
		dl = Directory::readDirectory(cache, new FileInputStream(cache, rootInode));

		for (unsigned long i = 0; i < dl->size(); i++)
			cout << dl->getEntry(i)->getEntryName() << endl;

		for (int i = 0; i < 300; i++)
			cout << i << ": " << map->isFree(i) << endl;
*/
		
/*
		Inode* fbInode = new Inode();
		fbInode->readFromHardDisk(cache, FILEATTR_AREA_START_ADDR+FREEBLOCKS_FILEATTR);
		BlockMap* map = new BlockMap(cache);

		FileOutputStream* fos = new FileOutputStream(cache, fbInode,
			FILEATTR_AREA_START_ADDR+FREEBLOCKS_FILEATTR, map);

		unsigned char* buf = new unsigned char[138753];
		for (int i = 0; i < 138752; i++)
			buf[i] = 'A';
		buf[138752] = 'B';

		fos->skip(22495);
		fos->write(buf, 138753); //138753
		cout << fbInode->getIndirectBlockAddress(0) << endl;
		cout << fbInode->getIndirectBlockAddress(1) << endl;
		cout << fbInode->getIndirectBlockAddress(2) << endl;
		cout << fbInode->getFileSizeInBytes() << endl;
		
		FileInputStream* fis = new FileInputStream(cache, fbInode);
		fis->skip(12495);
		fis->read(buf, 10001);

		for (unsigned long i = 0; i < 10001; i++)
			cout << buf[i] << " " ;
*/

/*
		PathParser* prs = new PathParser("../usr/local/share/doc/bin/");

		cout << "path is: " << prs->getPath() << endl;
		while(prs->hasMore())
			cout << prs->next() << endl;
		if (prs->startsWithPathSeparator())
			cout << "starts with separator" << endl;
		if (prs->endsWithPathSeparator())
			cout << "ends with separator" << endl;
*/
	}
	catch (IOException* e)
	{
		cout << e->getMessage() << endl;
	}
	catch (...)
	{
		cout << "An unknown exception has been caught." << endl;
	}
}
