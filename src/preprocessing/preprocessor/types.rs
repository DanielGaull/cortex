use crate::{
    interpreting::error::CortexError,
    parsing::codegen::r#trait::SimpleCodeGen,
    preprocessing::{
        ast::r#type::{RFollowsClause, RFollowsEntry, RType, RTypeArg},
        error::PreprocessingError,
        preprocessor::preprocessor::CortexPreprocessor,
    },
    r#type::r#type::{
        FollowsClause, FollowsEntry, FollowsType, FunctionType, PType, TypeArg, TypeParam,
    },
};

impl CortexPreprocessor {
    pub fn validate_type(&self, typ: PType) -> Result<RType, CortexError> {
        match typ {
            PType::BasicType(b) => {
                let result = self.get_struct_stub(&b.name);
                if result.is_none() {
                    return Err(Box::new(PreprocessingError::TypeDoesNotExist(
                        b.name.codegen(0),
                    )));
                }
                let (type_params, path) = result.unwrap();
                let type_args =
                    self.validate_type_args(&type_params, b.type_args, path.codegen(0), "Type")?;

                Ok(RType::BasicType(path, type_args))
            }
            PType::RefType(r) => Ok(RType::RefType(
                Box::new(self.validate_type(*r.contained)?),
                r.mutable,
            )),
            PType::TupleType(t) => {
                let mut types = Vec::new();
                for ty in t.types {
                    types.push(self.validate_type(ty)?);
                }
                Ok(RType::TupleType(types))
            }
            PType::FollowsType(f) => {
                let mut entries = Vec::new();
                for entry in f.clause.contracts {
                    let result = self.get_contract_stub(&entry.name);
                    if result.is_none() {
                        return Err(Box::new(PreprocessingError::ContractDoesNotExist(
                            entry.name.codegen(0),
                        )));
                    }
                    let (contract_type_params, path) = result.unwrap();
                    let type_args = self.validate_type_args(
                        contract_type_params,
                        entry.type_args,
                        path.codegen(0),
                        "Contract",
                    )?;
                    entries.push(RFollowsEntry {
                        name: path,
                        type_args,
                    })
                }
                Ok(RType::FollowsType(RFollowsClause { entries }))
            }
            PType::OptionalType(inner) => {
                Ok(RType::OptionalType(Box::new(self.validate_type(*inner)?)))
            }
            PType::NoneType => Ok(RType::NoneType),
            PType::ThisType => Ok(RType::ThisType),
            PType::GenericType(name) => Ok(RType::GenericType(name)),
            PType::FunctionType(f) => Ok(RType::FunctionType(
                f.type_params,
                f.param_types
                    .into_iter()
                    .map(|p| self.validate_type(p))
                    .collect::<Result<Vec<_>, _>>()?,
                Box::new(self.validate_type(*f.return_type)?),
            )),
        }
    }
    pub(crate) fn validate_type_args(
        &self,
        type_params: &Vec<TypeParam>,
        type_args: Vec<TypeArg>,
        type_name: String,
        object_name: &'static str,
    ) -> Result<Vec<RTypeArg>, CortexError> {
        if type_params.len() != type_args.len() {
            return Err(Box::new(PreprocessingError::MismatchedTypeArgCount(
                type_name,
                type_params.len(),
                type_args.len(),
                object_name,
            )));
        }
        let mut result = Vec::new();
        for (tparam, targ) in type_params.iter().zip(type_args) {
            match (&tparam.typ, targ) {
                (crate::r#type::r#type::TypeParamType::Ty, TypeArg::Ty(t)) => {
                    result.push(RTypeArg::Ty(self.validate_type(t)?));
                }
                (crate::r#type::r#type::TypeParamType::Int, TypeArg::Int(v)) => {
                    result.push(RTypeArg::Int(v));
                }
                (first, second) => {
                    return Err(Box::new(PreprocessingError::MismatchedTypeArgument(
                        second.codegen(0),
                        first.codegen(0),
                    )));
                }
            }
        }
        Ok(result)
    }

    pub(crate) fn devalidate_type(&self, ty: RType) -> PType {
        match ty {
            RType::BasicType(name, type_args) => {
                PType::basic(name, self.devalidate_type_args(type_args))
            }
            RType::RefType(inner, mutable) => {
                PType::reference(self.devalidate_type(*inner), mutable)
            }
            RType::TupleType(types) => {
                PType::tuple(types.into_iter().map(|t| self.devalidate_type(t)).collect())
            }
            RType::FollowsType(follows) => PType::FollowsType(FollowsType {
                clause: self.devalidate_follows_clause(follows),
            }),
            RType::OptionalType(inner) => self.devalidate_type(inner.to_optional()),
            RType::NoneType => PType::NoneType,
            RType::ThisType => PType::ThisType,
            RType::GenericType(name) => PType::GenericType(name),
            RType::FunctionType(type_param_types, param_types, return_type) => {
                PType::FunctionType(FunctionType {
                    type_params: type_param_types,
                    param_types: param_types
                        .into_iter()
                        .map(|p| self.devalidate_type(p))
                        .collect(),
                    return_type: Box::new(self.devalidate_type(*return_type)),
                })
            }
        }
    }
    pub(crate) fn devalidate_type_args(&self, ta: Vec<RTypeArg>) -> Vec<TypeArg> {
        ta.into_iter()
            .map(|t| match t {
                RTypeArg::Ty(t) => TypeArg::Ty(self.devalidate_type(t)),
                RTypeArg::Int(v) => TypeArg::Int(v),
            })
            .collect()
    }
    pub(crate) fn devalidate_follows_clause(&self, f: RFollowsClause) -> FollowsClause {
        FollowsClause {
            contracts: f
                .entries
                .into_iter()
                .map(|e| FollowsEntry {
                    name: e.name,
                    type_args: self.devalidate_type_args(e.type_args),
                })
                .collect(),
        }
    }
}
