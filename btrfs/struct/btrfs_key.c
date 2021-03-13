#include "btrfs_key.h"

#include "../btrfs_debug.h"


#ifdef BTRFS_DEBUG
#define BTRFS_CONST_CASE(__const) \
    case __const: \
        btrfs_debug_printf("%s [%llu = 0x%llx]", #__const, (u64) __const, (u64) __const); \
        return

static void btrfs_key_print_objectid(u64 objectid) {
    switch (objectid) {
        BTRFS_CONST_CASE(BTRFS_ROOT_TREE_OBJECTID);
        BTRFS_CONST_CASE(BTRFS_EXTENT_TREE_OBJECTID);
        BTRFS_CONST_CASE(BTRFS_CHUNK_TREE_OBJECTID);
        BTRFS_CONST_CASE(BTRFS_DEV_TREE_OBJECTID);
        BTRFS_CONST_CASE(BTRFS_FS_TREE_OBJECTID);
        BTRFS_CONST_CASE(BTRFS_ROOT_TREE_DIR_OBJECTID);
        BTRFS_CONST_CASE(BTRFS_CSUM_TREE_OBJECTID);
        BTRFS_CONST_CASE(BTRFS_QUOTA_TREE_OBJECTID);
        BTRFS_CONST_CASE(BTRFS_UUID_TREE_OBJECTID);
        BTRFS_CONST_CASE(BTRFS_FREE_SPACE_TREE_OBJECTID);
        BTRFS_CONST_CASE(BTRFS_DEV_STATS_OBJECTID);
        BTRFS_CONST_CASE(BTRFS_BALANCE_OBJECTID);
        BTRFS_CONST_CASE(BTRFS_ORPHAN_OBJECTID);
        BTRFS_CONST_CASE(BTRFS_TREE_LOG_OBJECTID);
        BTRFS_CONST_CASE(BTRFS_TREE_LOG_FIXUP_OBJECTID);
        BTRFS_CONST_CASE(BTRFS_TREE_RELOC_OBJECTID);
        BTRFS_CONST_CASE(BTRFS_DATA_RELOC_TREE_OBJECTID);
        BTRFS_CONST_CASE(BTRFS_EXTENT_CSUM_OBJECTID);
        BTRFS_CONST_CASE(BTRFS_FREE_SPACE_OBJECTID);
        BTRFS_CONST_CASE(BTRFS_FREE_INO_OBJECTID);
        BTRFS_CONST_CASE(BTRFS_MULTIPLE_OBJECTIDS);
        BTRFS_CONST_CASE(BTRFS_FIRST_FREE_OBJECTID);
        BTRFS_CONST_CASE(BTRFS_LAST_FREE_OBJECTID);
    }

    btrfs_debug_printf("%llu", objectid);
}

static void btrfs_key_print_type(u64 type) {
    switch (type) {
        BTRFS_CONST_CASE(BTRFS_INODE_ITEM_KEY);
        BTRFS_CONST_CASE(BTRFS_INODE_REF_KEY);
        BTRFS_CONST_CASE(BTRFS_INODE_EXTREF_KEY);
        BTRFS_CONST_CASE(BTRFS_XATTR_ITEM_KEY);
        BTRFS_CONST_CASE(BTRFS_ORPHAN_ITEM_KEY);
        BTRFS_CONST_CASE(BTRFS_DIR_LOG_ITEM_KEY);
        BTRFS_CONST_CASE(BTRFS_DIR_LOG_INDEX_KEY);
        BTRFS_CONST_CASE(BTRFS_DIR_ITEM_KEY);
        BTRFS_CONST_CASE(BTRFS_DIR_INDEX_KEY);
        BTRFS_CONST_CASE(BTRFS_EXTENT_DATA_KEY);
        BTRFS_CONST_CASE(BTRFS_CSUM_ITEM_KEY);
        BTRFS_CONST_CASE(BTRFS_EXTENT_CSUM_KEY);
        BTRFS_CONST_CASE(BTRFS_ROOT_ITEM_KEY);
        BTRFS_CONST_CASE(BTRFS_ROOT_BACKREF_KEY);
        BTRFS_CONST_CASE(BTRFS_ROOT_REF_KEY);
        BTRFS_CONST_CASE(BTRFS_EXTENT_ITEM_KEY);
        BTRFS_CONST_CASE(BTRFS_METADATA_ITEM_KEY);
        BTRFS_CONST_CASE(BTRFS_TREE_BLOCK_REF_KEY);
        BTRFS_CONST_CASE(BTRFS_EXTENT_DATA_REF_KEY);
        BTRFS_CONST_CASE(BTRFS_EXTENT_REF_V0_KEY);
        BTRFS_CONST_CASE(BTRFS_SHARED_BLOCK_REF_KEY);
        BTRFS_CONST_CASE(BTRFS_SHARED_DATA_REF_KEY);
        BTRFS_CONST_CASE(BTRFS_BLOCK_GROUP_ITEM_KEY);
        BTRFS_CONST_CASE(BTRFS_FREE_SPACE_INFO_KEY);
        BTRFS_CONST_CASE(BTRFS_FREE_SPACE_EXTENT_KEY);
        BTRFS_CONST_CASE(BTRFS_FREE_SPACE_BITMAP_KEY);
        BTRFS_CONST_CASE(BTRFS_DEV_EXTENT_KEY);
        BTRFS_CONST_CASE(BTRFS_DEV_ITEM_KEY);
        BTRFS_CONST_CASE(BTRFS_CHUNK_ITEM_KEY);
        BTRFS_CONST_CASE(BTRFS_QGROUP_STATUS_KEY);
        BTRFS_CONST_CASE(BTRFS_QGROUP_INFO_KEY);
        BTRFS_CONST_CASE(BTRFS_QGROUP_LIMIT_KEY);
        BTRFS_CONST_CASE(BTRFS_QGROUP_RELATION_KEY);
        BTRFS_CONST_CASE(BTRFS_TEMPORARY_ITEM_KEY);
        BTRFS_CONST_CASE(BTRFS_PERSISTENT_ITEM_KEY);
        BTRFS_CONST_CASE(BTRFS_DEV_REPLACE_KEY);
    }

    btrfs_debug_printf("%llu", type);
}

void btrfs_key_print(struct btrfs_key * key) {
    btrfs_debug_indent();

    btrfs_debug_printf("(");
    btrfs_key_print_objectid(key->objectid);
    btrfs_debug_printf(", ");
    btrfs_key_print_type(key->type);
    btrfs_debug_printf(", %llu)\n", key->offset);
}
#endif