DROP TRIGGER IF EXISTS trigger_users_updated_at ON users;

CREATE TRIGGER trigger_users_updated_at
    BEFORE UPDATE ON users
    FOR EACH ROW
    EXECUTE FUNCTION set_updated_at();

DROP TRIGGER IF EXISTS trigger_maps_updated_at ON maps;

CREATE TRIGGER trigger_maps_updated_at
    BEFORE UPDATE ON maps
    FOR EACH ROW
    EXECUTE FUNCTION set_updated_at();

DROP TRIGGER IF EXISTS trigger_map_members_updated_at ON map_members;

CREATE TRIGGER trigger_map_members_updated_at
    BEFORE UPDATE ON map_members
    FOR EACH ROW
    EXECUTE FUNCTION set_updated_at();

DROP TRIGGER IF EXISTS trigger_nodes_updated_at ON nodes;

CREATE TRIGGER trigger_nodes_updated_at
    BEFORE UPDATE ON nodes
    FOR EACH ROW
    EXECUTE FUNCTION set_updated_at();
